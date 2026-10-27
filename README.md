# bpmn-guard

Deterministic pre-deployment linter for Camunda 7 BPMN 2.0 files. It parses the
XML, builds a JGraphT directed graph of every sequence flow, runs seven graph
rules that predict OOM / deadlock / thread-lock behaviour, renders the diagram
itself from the file's DI section with `java.awt`, overlays colour-coded
findings with rule badges, and ships a Swing desktop UI for the same engine.

No LLM, no network, no native binaries, no external renderer. Every rule is a
pure function `ProcessGraph -> [Violation]`.

```
src/bpmn_guard/
  model.clj    XML  -> flat model        (clojure.data.xml, namespace-agnostic, clojure.spec)
  di.clj       XML  -> {element-id bounds} from BPMNShape / BPMNEdge
  graph.clj    model -> JGraphT DirectedPseudograph + traversal primitives
  rules.clj    R1..R7 + rule catalogue (badge titles, colour class, remediation)
  render.clj   model + DI -> base diagram -> violation overlay   (BufferedImage / Graphics2D only)
  gui.clj      Swing desktop UI (JFileChooser, JSplitPane, JScrollPane canvas, JTable)
  core.clj     validate / JSON / dual-mode CLI / exit codes
resources/samples/risky-order.bpmn   every rule fires on this file
test/bpmn_guard/rules_test.clj
```

## Run

```bash
clj -M:test                                        # unit tests against the sample
clj -M:run                                         # desktop UI
clj -M:run resources/samples/risky-order.bpmn      # headless text report, exit 2 on :critical
clj -M:run order.bpmn order.png                    # headless + annotated PNG drawn from the DI
clj -M:run order.bpmn order.png --json --scale 1.5 # JSON on stdout, 1.5x image
clj -T:build uber
java -jar target/bpmn-guard.jar                    # UI
java -jar target/bpmn-guard.jar order.bpmn out.png # CI
```

The UI: **Open .BPMN file…** validates and renders immediately; the top pane is
the scrollable annotated diagram, the bottom pane is a sortable table of
Rule · Severity · Element ID · Element Name · Type · Remediation. Clicking a
row scrolls the diagram to that element. **Export annotated PNG…** saves the
current canvas.

From Clojure / another JVM tool:

```clojure
(require '[bpmn-guard.core :as guard] '[bpmn-guard.render :as render])

(def report (guard/validate "order.bpmn" {:chain-threshold 5}))
(:summary report)      ;=> {:critical 4 :high 2 :medium 0}
(first (:violations report))
;=> {:rule :R4 :severity :critical :element-id "JoinApproval" :element-type "parallelGateway"
;    :message "..." :related ["FraudCheck" "f3" "f4"]
;    :bounds {:x 550.0 :y 175.0 :width 50.0 :height 50.0 :kind :shape}
;    :related-bounds {...} :evidence {...}}

(render/render-to-file report "order-annotated.png" {:scale 1.0})   ; from the DI alone
(render/render-report report {})                                      ; -> BufferedImage
(render/annotate-file "base.png" "out.png" report)                    ; legacy: external base PNG
```

`guard/validate` accepts anything `clojure.java.io/reader` accepts (path, File,
InputStream, Reader, URL). Unreadable input or malformed XML throws `ex-info`
with `:type :bpmn-guard.model/io-error` or `:xml-error`; missing DI is a
warning and leaves `:bounds nil`.

## Rules

| Rule | Severity | Detection |
|---|---|---|
| **R1** Sequential MI variable accumulation | critical / high | For every container with `isSequential="true"`: finds every statically visible non-local write in the body — `camunda:resultVariable`, `camunda:outputParameter` (plain and connector), and `execution./connector.setVariable(` in any script (listeners, input/output mappings, connector mappings); `setVariableLocal` and `inputParameter` are exempt. Writes whose name or value looks like an API response / request body are marked payload-sized. Emits one **summary** on the container (write count, payload count, inner-loop multipliers, formula `|collection| × (writes + retries×re-executed)`), one finding per writing activity, and one **grouped** finding per inner cycle stating its back-edge condition and whether it is counter-bounded. |
| **R2** Scope bypass / orphaned tokens | critical / high | (a) sequence flow whose source and target are in different scopes; (b) boundary event declared in a different container than its host; (c) boundary-event path that re-enters its host's body without passing through the host; (d) non-end node with no outgoing flow (token dies silently; event sub-processes and compensation handlers exempt). |
| **R3** Synchronous infinite loop | critical | Delete every wait state (`userTask`, `receiveTask`, `intermediateCatchEvent`, `eventBasedGateway`, timer/message/signal/conditional boundary, `camunda:type="external"`) and every node with `asyncBefore`/`asyncAfter`. Any strongly connected component left (Kosaraju) is a cycle with zero commits. A bounded DFS then enumerates sample cycles per SCC (bounded by `:max-scc-enumeration` / `:max-cycles-per-scc`). Boundary events get a synthetic `host -> boundary` edge so error-boundary retry loops are caught. |
| **R4** XOR/OR split into AND join | critical / high / medium | For each diverging `exclusiveGateway`/`inclusiveGateway`, compute the reachable set of each branch (BFS, never re-entering the split). For each `parallelGateway` join inside that cone, count which join inputs each branch feeds. If no single branch guarantees every input, the join deadlocks. Critical when two or more XOR branches each cover a strict subset; downgraded one level for inclusive splits. |
| **R5** Parallel MI without save point | critical | `multiInstanceLoopCharacteristics` with `isSequential` false/absent and no `camunda:asyncBefore="true"` on the activity. |
| **R7** Event-based gateway target | critical | Every immediate target of an `eventBasedGateway` must be a `receiveTask` or an `intermediateCatchEvent` with a message/timer/signal/conditional definition; anything else (task, gateway, throw event) is flagged. |
| **R6** Unbroken compute chain | high / critical | Chain graph = `serviceTask`/`scriptTask`/`businessRuleTask`/`sendTask` (external tasks excluded), keeping edge `u -> v` only when `u` lacks `asyncAfter` and `v` lacks `asyncBefore`. If acyclic: longest-path DP in topological order, report every maximal chain of length ≥ `:chain-threshold` (default 5). If cyclic: the SCC is an unbounded chain. |

All thresholds live in `bpmn-guard.rules/default-opts` and can be overridden per call.

## Rendering

`render/render-diagram` draws the whole diagram from the `.bpmn` file:

* canvas = DI extent (shapes, waypoints, labels) × `:scale` + 2 × `:padding`, white background, antialiased;
* activities as light-grey rounded rectangles (thicker border for call activities, `|||` / `≡` multi-instance markers, `+` for collapsed sub-processes), gateways as diamonds with X / + / ○ / double-circle markers, events as circles (thick for end, double for intermediate/boundary) with primitive-drawn timer / message / signal / terminate markers;
* sequence flows as polylines through their `di:waypoint`s with a filled arrowhead, a slash for default flows and a diamond for conditional flows;
* names centred inside activities, along the top edge of expanded sub-processes, and at the DI label position (or just below) for events and gateways.

`render/overlay-violations!` then colours each finding by rule class and adds a
high-contrast badge such as `[R3: Sync Loop]`, stacked when several rules hit
one element:

| Colour | Rules | Meaning |
|---|---|---|
| Red | R1, R3, R5 | OOM / crash |
| Orange | R2, R4, R7 | deadlock / orphaned tokens / engine fault |
| Amber | R6 | transaction starvation |

Activities get a tight rounded-rectangle halo, events and gateways a circle;
flagged sequence flows are re-stroked thick in colour along their waypoints.
Grouped findings (R1 inner loops) are drawn once as a dashed outline around
the whole cycle with a single `[R1: loop ×n]` badge instead of a badge on
every node. One badge per element lists every rule that hit it
(`[R1+R5: …]`). Text annotations and associations are drawn too.

## Jenkins

```groovy
stage('BPMN lint') {
  steps {
    sh 'java -jar bpmn-guard.jar src/main/resources/processes/order.bpmn order-lint.png --json > bpmn-report.json'
    archiveArtifacts artifacts: 'order-lint.png, bpmn-report.json'
    // exit 2 => critical finding, fails the stage; 1 => high (with --fail-on high)
  }
}
```

## Known limits

* The file chooser starts in the last directory used, then `$BPMN_GUARD_DIR`,
  then — under WSL — `/mnt/c/Users/<user>`, then the JVM's `user.home`. Hidden
  (dot) directories are shown.
* Static analysis only: R4 does not evaluate conditions, so an XOR whose
  branches are mutually exhaustive *and* both reach the join is still flagged —
  that is the intended behaviour (it is still a deadlock on the untaken branch).
* R1 sees variable writes in scripts via a regex on `setVariable(`; writes done
  inside Java delegates are invisible to any static tool, hence
  `camunda:resultVariable` / `outputParameter` are the signals it relies on.
* R3 treats `camunda:asyncBefore` on a multi-instance activity as a commit at
  the multi-instance body, matching the attribute's documented default.
