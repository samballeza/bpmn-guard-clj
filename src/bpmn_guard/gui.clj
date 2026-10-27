(ns bpmn-guard.gui
  "Interactive Swing front-end.

     (bpmn-guard.gui/launch!)          ; opens the window
     (bpmn-guard.gui/launch! \"x.bpmn\") ; opens and immediately validates a file

   Layout: toolbar (Open / Export PNG / zoom) over a JSplitPane whose top is
   the scrollable annotated diagram and whose bottom is the violation table.
   Selecting a table row scrolls the diagram to that element."
  (:require [bpmn-guard.core :as core]
            [bpmn-guard.render :as render]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.awt BorderLayout Color Cursor Dimension Font Graphics Graphics2D Rectangle RenderingHints]
           [java.awt.event ActionListener ComponentAdapter MouseAdapter MouseEvent]
           [java.awt.image BufferedImage]
           [java.io File]
           [javax.swing BorderFactory JButton JFileChooser JFrame JLabel JOptionPane JPanel
            JScrollPane JSplitPane JTable JTextArea JToolBar ListSelectionModel SwingUtilities
            UIManager SwingConstants]
           [javax.swing.event ListSelectionListener TableColumnModelListener TableModelListener]
           [javax.swing.filechooser FileNameExtensionFilter]
           [javax.swing.table DefaultTableModel DefaultTableCellRenderer TableCellRenderer]))

(def ^:private columns
  (into-array Object ["Rule" "Severity" "Element ID" "Element Name" "Type" "Finding" "Remediation" "#"]))

(def ^:private wrapped-columns #{5 6})   ; rendered multi-line; row height follows their wrapped text

(def ^:private zoom-levels [0.5 0.75 1.0 1.25 1.5 2.0 2.5 3.0])

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defn- new-state []
  (atom {:file nil :report nil :image nil :scale 1.5 :regions []}))

;; ---------------------------------------------------------------------------
;; Start directory for the file chooser
;;
;; Swing's chooser starts in user.home, which on WSL / a container is the
;; Linux home (/home/<user>), not the Windows profile the person is looking
;; at in Explorer.  Resolve, in order: last directory used (java.util.prefs),
;; $BPMN_GUARD_DIR, the Windows profile when running under WSL, user.home.
;; ---------------------------------------------------------------------------

(def ^:private prefs (java.util.prefs.Preferences/userRoot))
(def ^:private prefs-node "bpmn-guard")
(def ^:private last-dir-key "last-dir")

(defn- existing-dir ^File [^String path]
  (when (and path (not (str/blank? path)))
    (let [f (File. path)]
      (when (.isDirectory f) f))))

(defn- wsl-windows-home ^File []
  (when (or (System/getenv "WSL_DISTRO_NAME") (.isDirectory (File. "/mnt/c/Users")))
    (let [user (or (System/getenv "USER") (System/getProperty "user.name"))
          cand [(str "/mnt/c/Users/" user) "/mnt/c/Users"]]
      (some existing-dir cand))))

(defn- start-dir ^File []
  (or (existing-dir (.get (.node prefs prefs-node) last-dir-key nil))
      (existing-dir (System/getenv "BPMN_GUARD_DIR"))
      (wsl-windows-home)
      (existing-dir (System/getProperty "user.home"))
      (File. ".")))

(defn- remember-dir! [^File f]
  (when-let [d (if (.isDirectory f) f (.getParentFile f))]
    (.put (.node prefs prefs-node) last-dir-key (.getAbsolutePath d))))

;; ---------------------------------------------------------------------------
;; Widgets
;; ---------------------------------------------------------------------------

(defn- image-panel
  "JPanel that paints the BufferedImage held in state (or a hint when empty)."
  ^JPanel [state]
  (proxy [JPanel] []
    (getPreferredSize []
      (if-let [^BufferedImage img (:image @state)]
        (Dimension. (.getWidth img) (.getHeight img))
        (Dimension. 900 500)))
    (paintComponent [^Graphics g]
      (let [^JPanel this this]
        (proxy-super paintComponent g)
        (let [g2 ^Graphics2D g]
          (.setRenderingHint g2 RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
          (if-let [^BufferedImage img (:image @state)]
            (.drawImage g2 img 0 0 nil)
            (do (.setColor g2 (Color. 120 120 120))
                (.drawString g2 "Open a .bpmn file to validate and render it."
                             30 (int (/ (.getHeight this) 2))))))))))

(defn- table-model ^DefaultTableModel []
  (proxy [DefaultTableModel] [columns 0]
    (isCellEditable [_ _] false)
    (getColumnClass [col] (if (= col 7) Integer String))))

(defn- severity-renderer ^DefaultTableCellRenderer []
  (proxy [DefaultTableCellRenderer] []
    (getTableCellRendererComponent [table value selected? focus? row col]
      (let [^JLabel c (proxy-super getTableCellRendererComponent table value selected? focus? row col)
            ^JTable table table
            rule  (keyword (str (.getValueAt (.getModel table) (.convertRowIndexToModel table row) 0)))
            color (render/rule-color rule)]
        (when-not selected?
          (.setForeground c color))
        (.setHorizontalAlignment c SwingConstants/CENTER)
        c))))

(defn- wrapping-renderer
  "Cell renderer that word-wraps long text; the table's row heights are
   recomputed from these renderers (see adjust-row-heights!)."
  ^TableCellRenderer []
  (let [area (doto (JTextArea.)
               (.setLineWrap true)
               (.setWrapStyleWord true)
               (.setOpaque true)
               (.setBorder (BorderFactory/createEmptyBorder 3 6 3 6)))]
    (reify TableCellRenderer
      (getTableCellRendererComponent [_ table value selected? _ _ _]
        (let [^JTable table table]
          (.setText area (str value))
          (.setFont area (.getFont table))
          (if selected?
            (do (.setBackground area (.getSelectionBackground table))
                (.setForeground area (.getSelectionForeground table)))
            (do (.setBackground area (.getBackground table))
                (.setForeground area (.getForeground table))))
          area)))))

(defn adjust-row-heights!
  "Give every row the height its tallest wrapped cell needs at the current
   column widths."
  [^JTable table]
  (dotimes [row (.getRowCount table)]
    (let [h (reduce (fn [acc col]
                      (let [vc (.convertColumnIndexToView table col)]
                        (if (neg? vc)
                          acc
                          (let [r (.getCellRenderer table row vc)
                                c (.prepareRenderer table r row vc)
                                w (.getWidth (.getColumn (.getColumnModel table) vc))]
                            (.setSize c w 1000)
                            (max acc (.height (.getPreferredSize c)))))))
                    22
                    wrapped-columns)]
      (when (not= h (.getRowHeight table row))
        (.setRowHeight table row h)))))

(defn- kind-label [v]
  (if-let [k (:kind v)] (format "[%s] " (name k)) ""))

(defn- fill-table! [^DefaultTableModel model report]
  (.setRowCount model 0)
  (doseq [[i v] (map-indexed vector (:violations report))]
    (.addRow model (into-array Object [(name (:rule v))
                                       (str/upper-case (name (:severity v)))
                                       (:element-id v)
                                       (or (:element-name v) "")
                                       (or (:element-type v) "")
                                       (str (kind-label v) (:message v))
                                       (:remediation v)
                                       (int i)]))))

(defn- selected-violation
  "The violation behind the selected row (rows are mapped by the hidden index
   column, so two findings on the same element never collide)."
  [state ^JTable table]
  (let [row (.getSelectedRow table)]
    (when (>= row 0)
      (let [mrow (.convertRowIndexToModel table row)
            idx  (.getValueAt (.getModel table) mrow 7)]
        (get-in @state [:report :violations idx])))))

(defn- jsonable
  "Keywords become strings, sets become arrays; everything else data.json handles."
  [x]
  (cond
    (map? x)     (into (sorted-map) (map (fn [[k v]] [(if (keyword? k) (name k) (str k)) (jsonable v)]) x))
    (set? x)     (mapv jsonable (sort-by str x))
    (sequential? x) (mapv jsonable x)
    (keyword? x) (name x)
    :else        x))

(defn- evidence-text [evidence]
  (with-out-str (json/pprint (jsonable evidence) :escape-slash false)))

(defn- detail-text [v]
  (if-not v
    "Select a finding to see the full assessment."
    (str (format "%s %s · %s · %s%s\n"
                 (name (:rule v)) (kind-label v) (str/upper-case (name (:severity v)))
                 (:element-id v)
                 (if-let [n (:element-name v)] (str " — " n) ""))
         (format "%s in scope %s\n\n" (or (:element-type v) "") (or (:scope v) "process"))
         "FINDING\n" (:message v) "\n\n"
         "REMEDIATION\n" (:remediation v) "\n\n"
         (when (seq (:related v))
           (str "RELATED ELEMENTS\n" (str/join ", " (:related v)) "\n\n"))
         "EVIDENCE\n" (evidence-text (:evidence v)))))

(defn- summary-text [report]
  (let [{:keys [critical high medium]} (:summary report)]
    (format "%s  —  %d critical, %d high, %d medium%s"
            (:source report) critical high medium
            (if (seq (:warnings report))
              (str "   |   " (str/join " · " (:warnings report)))
              ""))))

(defn- choose-file ^File [parent mode]
  (let [fc (doto (JFileChooser. (start-dir))
             (.setFileHidingEnabled false)
             (.setFileFilter (if (= mode :open)
                               (FileNameExtensionFilter. "BPMN files (*.bpmn, *.xml)" (into-array String ["bpmn" "xml"]))
                               (FileNameExtensionFilter. "PNG image (*.png)" (into-array String ["png"])))))
        res (if (= mode :open) (.showOpenDialog fc parent) (.showSaveDialog fc parent))]
    (when (= res JFileChooser/APPROVE_OPTION)
      (let [f (.getSelectedFile fc)]
        (remember-dir! f)
        (if (and (= mode :save) (not (str/ends-with? (str/lower-case (.getName f)) ".png")))
          (File. (str (.getPath f) ".png"))
          f)))))

(defn- error! [parent title ^Throwable e]
  (JOptionPane/showMessageDialog parent (.getMessage e) title JOptionPane/ERROR_MESSAGE))

;; ---------------------------------------------------------------------------
;; Actions
;; ---------------------------------------------------------------------------

(defn- render-current!
  "Re-render the annotated image at the current scale (on the EDT)."
  [state ^JPanel canvas]
  (when-let [report (:report @state)]
    (let [opts {:scale (:scale @state)}
          img  (when (seq (:di report)) (render/render-report report opts))
          regs (if img (render/hit-regions report opts) [])]
      (swap! state assoc :image img :regions regs)
      (.revalidate canvas)
      (.repaint canvas))))

(defn- load-file!
  "Validate `file` off the EDT, then publish report + image to the widgets."
  [state ^JFrame frame ^JPanel canvas ^DefaultTableModel model ^JLabel status file]
  (.setText status (str "Validating " file " …"))
  (future
    (try
      (let [report (core/validate (str file))]
        (SwingUtilities/invokeLater
         (fn []
           (swap! state assoc :file file :report report)
           (fill-table! model report)
           (.setText status (summary-text report))
           (render-current! state canvas)
           (.setTitle frame (str "bpmn-guard — " (.getName ^File file))))))
      (catch Exception e
        (SwingUtilities/invokeLater
         (fn []
           (.setText status (str "Failed: " (.getMessage e)))
           (error! frame "Cannot validate file" e)))))))

(defn- export!
  [state ^JFrame frame]
  (if-let [^BufferedImage img (:image @state)]
    (when-let [f (choose-file frame :save)]
      (try
        (render/write-png img f)
        (JOptionPane/showMessageDialog frame (str "Saved " (.getPath f)) "Export" JOptionPane/INFORMATION_MESSAGE)
        (catch Exception e (error! frame "Export failed" e))))
    (JOptionPane/showMessageDialog frame "Nothing rendered yet." "Export" JOptionPane/WARNING_MESSAGE)))

(defn- scroll-to-selection!
  "Scroll the canvas so the element of the selected table row is visible."
  [state ^JTable table ^JPanel canvas]
  (let [report (:report @state)
        v      (selected-violation state table)]
    (when-let [b (:bounds v)]
      (let [{:keys [tx ty ts]} (render/make-transform (:di report)
                                                      (merge render/default-opts {:scale (:scale @state)}))
            pad 80]
        (.scrollRectToVisible canvas
                              (Rectangle. (int (- (tx (:x b)) pad)) (int (- (ty (:y b)) pad))
                                          (int (+ (ts (:width b)) (* 2 pad)))
                                          (int (+ (ts (:height b)) (* 2 pad)))))))))

(defn- zoom! [state canvas direction]
  (let [cur (:scale @state)
        idx (.indexOf ^java.util.List zoom-levels cur)
        nxt (get zoom-levels (-> idx (+ direction) (max 0) (min (dec (count zoom-levels)))) cur)]
    (swap! state assoc :scale nxt)
    (render-current! state canvas)))

(defn- select-violation!
  "Select the table row that shows violation `idx` (by the hidden index column)."
  [^JTable table idx]
  (let [model (.getModel table)]
    (when-let [mrow (first (filter #(= idx (.getValueAt model % 7)) (range (.getRowCount model))))]
      (let [vrow (.convertRowIndexToView table mrow)]
        (when (>= vrow 0)
          (.setRowSelectionInterval table vrow vrow)
          (.scrollRectToVisible table (.getCellRect table vrow 0 true)))))))

(defn- install-canvas-picking!
  "Click a highlighted element or loop outline on the diagram to select its finding."
  [state ^JPanel canvas ^JTable table]
  (.addMouseListener canvas
                     (proxy [MouseAdapter] []
                       (mouseClicked [^MouseEvent e]
                         (when-let [idx (render/hit-test (:regions @state) (.getX e) (.getY e))]
                           (select-violation! table idx)))))
  (.addMouseMotionListener canvas
                           (proxy [MouseAdapter] []
                             (mouseMoved [^MouseEvent e]
                               (.setCursor canvas
                                           (if (render/hit-test (:regions @state) (.getX e) (.getY e))
                                             (Cursor/getPredefinedCursor Cursor/HAND_CURSOR)
                                             (Cursor/getDefaultCursor)))))))

(defn- button ^JButton [^String text f]
  (doto (JButton. text)
    (.addActionListener (reify ActionListener (actionPerformed [_ _] (f))))))

;; ---------------------------------------------------------------------------
;; Frame
;; ---------------------------------------------------------------------------

(defn build-frame
  "Construct (but do not show) the main window.
   Returns {:frame JFrame, :loader (fn [file])}."
  [state]
  (let [frame   (JFrame. "bpmn-guard")
        canvas  (image-panel state)
        scroll  (JScrollPane. canvas)
        model   (table-model)
        table   (doto (JTable. model)
                  (.setAutoCreateRowSorter true)
                  (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
                  (.setRowHeight 22)
                  (.setAutoResizeMode JTable/AUTO_RESIZE_LAST_COLUMN)
                  (.setFillsViewportHeight true))
        detail  (doto (JTextArea. "Select a finding to see the full assessment.")
                  (.setEditable false)
                  (.setLineWrap true)
                  (.setWrapStyleWord true)
                  (.setMargin (java.awt.Insets. 8 10 8 10))
                  (.setFont (Font. Font/SANS_SERIF Font/PLAIN 13)))
        status  (doto (JLabel. "No file loaded.")
                  (.setBorder (BorderFactory/createEmptyBorder 4 8 4 8)))
        toolbar (doto (JToolBar.) (.setFloatable false))
        lower   (doto (JSplitPane. JSplitPane/HORIZONTAL_SPLIT (JScrollPane. table) (JScrollPane. detail))
                  (.setResizeWeight 0.6)
                  (.setOneTouchExpandable true))
        split   (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT scroll lower)
                  (.setResizeWeight 0.55)
                  (.setOneTouchExpandable true))
        wrap    (wrapping-renderer)
        cm      (.getColumnModel table)]
    (doto cm
      (-> (.getColumn 0) (.setMaxWidth 60))
      (-> (.getColumn 0) (.setCellRenderer (severity-renderer)))
      (-> (.getColumn 1) (.setMaxWidth 90))
      (-> (.getColumn 2) (.setPreferredWidth 150))
      (-> (.getColumn 3) (.setPreferredWidth 170))
      (-> (.getColumn 4) (.setPreferredWidth 110))
      (-> (.getColumn 5) (.setPreferredWidth 520))
      (-> (.getColumn 5) (.setCellRenderer wrap))
      (-> (.getColumn 6) (.setPreferredWidth 420))
      (-> (.getColumn 6) (.setCellRenderer wrap)))
    ;; hide the index column from view (it stays in the model for row -> violation mapping)
    (.removeColumn cm (.getColumn cm 7))
    ;; keep wrapped rows the right height when columns are resized or the model changes
    (.addColumnModelListener cm
                             (reify TableColumnModelListener
                               (columnMarginChanged [_ _] (adjust-row-heights! table))
                               (columnAdded [_ _]) (columnRemoved [_ _])
                               (columnMoved [_ _]) (columnSelectionChanged [_ _])))
    (.addTableModelListener model
                            (reify TableModelListener
                              (tableChanged [_ _] (SwingUtilities/invokeLater #(adjust-row-heights! table)))))
    (.addComponentListener table
                           (proxy [ComponentAdapter] []
                             (componentResized [_] (adjust-row-heights! table))))
    (install-canvas-picking! state canvas table)
    (.addListSelectionListener (.getSelectionModel table)
                               (reify ListSelectionListener
                                 (valueChanged [_ e]
                                   (when-not (.getValueIsAdjusting e)
                                     (.setText detail (detail-text (selected-violation state table)))
                                     (.setCaretPosition detail 0)
                                     (scroll-to-selection! state table canvas)))))
    (doto toolbar
      (.add (button "Open .BPMN file…"
                    #(when-let [f (choose-file frame :open)]
                       (load-file! state frame canvas model status f))))
      (.add (button "Export annotated PNG…" #(export! state frame)))
      (.addSeparator)
      (.add (button "Zoom −" #(zoom! state canvas -1)))
      (.add (button "Zoom +" #(zoom! state canvas 1)))
      (.addSeparator)
      (.add (JLabel. "  Red: OOM/crash   Orange: deadlock/orphaned tokens   Amber: transaction starvation")))
    (doto (.getContentPane frame)
      (.setLayout (BorderLayout.))
      (.add toolbar BorderLayout/NORTH)
      (.add split BorderLayout/CENTER)
      (.add status BorderLayout/SOUTH))
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize 1440 900)
      (.setLocationRelativeTo nil))
    {:frame frame
     :loader (fn [f] (load-file! state frame canvas model status f))}))

(defn launch!
  "Show the window on the EDT.  With a file argument, validates it immediately.
   Returns the JFrame."
  ([] (launch! nil))
  ([file]
   (try (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
        (catch Exception _ nil))
   (let [state  (new-state)
         result (promise)]
     (SwingUtilities/invokeAndWait
      (fn []
        (let [{:keys [^JFrame frame loader]} (build-frame state)]
          (.setVisible frame true)
          (when file (loader (io/file file)))
          (deliver result frame))))
     @result)))
