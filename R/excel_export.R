# Build the Excel desk workbook with openxlsx, then inject a native OOXML line
# chart. openxlsx 4.2.9 does not expose a chart API, so the chart is added as a
# first-class chart part (not an image) after openxlsx saves the workbook.

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

chart_xml <- function(sheet_name, n_rows, series_cols, series_names,
                      colours, title) {
  last_row <- n_rows + 1L
  ser_xml <- vapply(seq_along(series_cols), function(i) {
    col_letter <- openxlsx::int2col(series_cols[[i]])
    paste0(
      '<c:ser><c:idx val="', i - 1, '"/><c:order val="', i - 1, '"/>',
      '<c:tx><c:strRef><c:f>', xml_escape(sheet_name), '!$', col_letter,
      '$1</c:f><c:strCache><c:ptCount val="1"/><c:pt idx="0"><c:v>',
      xml_escape(series_names[[i]]), '</c:v></c:pt></c:strCache></c:strRef></c:tx>',
      '<c:spPr><a:ln w="22225"><a:solidFill><a:srgbClr val="',
      colours[[i]], '"/></a:solidFill></a:ln></c:spPr>',
      '<c:marker><c:symbol val="none"/></c:marker>',
      '<c:cat><c:numRef><c:f>', xml_escape(sheet_name), '!$A$2:$A$', last_row,
      '</c:f></c:numRef></c:cat>',
      '<c:val><c:numRef><c:f>', xml_escape(sheet_name), '!$', col_letter,
      '$2:$', col_letter, '$', last_row, '</c:f></c:numRef></c:val>',
      '<c:smooth val="0"/></c:ser>'
    )
  }, character(1))

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<c:date1904 val="0"/><c:lang val="en-US"/><c:roundedCorners val="0"/>',
    '<c:chart>',
    '<c:title><c:tx><c:rich><a:bodyPr/><a:lstStyle/><a:p><a:r><a:rPr lang="en-US" b="1"/>',
    '<a:t>', xml_escape(title), '</a:t></a:r></a:p></c:rich></c:tx>',
    '<c:overlay val="0"/></c:title><c:autoTitleDeleted val="0"/>',
    '<c:plotArea><c:layout/><c:lineChart><c:grouping val="standard"/>',
    '<c:varyColors val="0"/>', paste(ser_xml, collapse = ""),
    '<c:marker val="1"/><c:smooth val="0"/>',
    '<c:axId val="100000001"/><c:axId val="100000002"/></c:lineChart>',
    '<c:catAx><c:axId val="100000001"/><c:scaling><c:orientation val="minMax"/></c:scaling>',
    '<c:delete val="0"/><c:axPos val="b"/><c:numFmt formatCode="yyyy-mm-dd" sourceLinked="0"/>',
    '<c:majorTickMark val="out"/><c:minorTickMark val="none"/><c:tickLblPos val="nextTo"/>',
    '<c:crossAx val="100000002"/><c:crosses val="autoZero"/><c:auto val="1"/>',
    '<c:lblAlgn val="ctr"/><c:lblOffset val="100"/><c:noMultiLvlLbl val="0"/></c:catAx>',
    '<c:valAx><c:axId val="100000002"/><c:scaling><c:orientation val="minMax"/></c:scaling>',
    '<c:delete val="0"/><c:axPos val="l"/><c:majorGridlines/>',
    '<c:numFmt formatCode="0.00" sourceLinked="1"/><c:majorTickMark val="out"/>',
    '<c:minorTickMark val="none"/><c:tickLblPos val="nextTo"/>',
    '<c:crossAx val="100000001"/><c:crosses val="autoZero"/><c:crossBetween val="between"/>',
    '</c:valAx></c:plotArea>',
    '<c:legend><c:legendPos val="b"/><c:overlay val="0"/></c:legend>',
    '<c:plotVisOnly val="1"/><c:dispBlanksAs val="gap"/><c:showDLblsOverMax val="0"/>',
    '</c:chart></c:chartSpace>'
  )
}

drawing_xml <- function(rel_id, anchor_col = 7, anchor_row = 2,
                        width_cols = 10, height_rows = 22) {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
    '<xdr:twoCellAnchor>',
    '<xdr:from><xdr:col>', anchor_col, '</xdr:col><xdr:colOff>0</xdr:colOff>',
    '<xdr:row>', anchor_row, '</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:from>',
    '<xdr:to><xdr:col>', anchor_col + width_cols, '</xdr:col><xdr:colOff>0</xdr:colOff>',
    '<xdr:row>', anchor_row + height_rows, '</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>',
    '<xdr:graphicFrame macro=""><xdr:nvGraphicFramePr>',
    '<xdr:cNvPr id="2" name="Equity Curve Chart"/><xdr:cNvGraphicFramePr/>',
    '</xdr:nvGraphicFramePr>',
    '<xdr:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/></xdr:xfrm>',
    '<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/chart">',
    '<c:chart xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
    'r:id="', rel_id, '"/></a:graphicData></a:graphic></xdr:graphicFrame>',
    '<xdr:clientData/></xdr:twoCellAnchor></xdr:wsDr>'
  )
}

rels_xml <- function(id, type, target) {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="', id, '" Type="', type, '" Target="', target, '"/>',
    '</Relationships>'
  )
}

# openxlsx reserves drawing/vml relationships on every sheet without creating
# the parts. Strict readers (openpyxl) follow those relationships and fail, so
# remove relationships and content-type overrides that point at missing files.
prune_missing_parts <- function(tmp) {
  rels_files <- list.files(file.path(tmp, "xl"), pattern = "\\.rels$",
                           recursive = TRUE, full.names = TRUE)
  for (f in rels_files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "")
    tags <- regmatches(txt, gregexpr("<Relationship[^>]*/>", txt))[[1]]
    if (length(tags) == 0) next
    base <- dirname(dirname(f))
    keep <- vapply(tags, function(tag) {
      if (grepl('TargetMode="External"', tag, fixed = TRUE)) return(TRUE)
      target <- sub('.*Target="([^"]+)".*', "\\1", tag)
      if (grepl("^[a-zA-Z]+://", target)) return(TRUE)
      file.exists(normalizePath(file.path(base, target), mustWork = FALSE))
    }, logical(1))
    if (all(keep)) next
    body <- paste(tags[keep], collapse = "")
    writeLines(
      paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
             '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
             body, "</Relationships>"),
      f, useBytes = TRUE
    )
  }

  ct_file <- file.path(tmp, "[Content_Types].xml")
  ct <- paste(readLines(ct_file, warn = FALSE), collapse = "")
  tags <- regmatches(ct, gregexpr("<Override[^>]*/>", ct))[[1]]
  keep <- vapply(tags, function(tag) {
    part <- sub('.*PartName="([^"]+)".*', "\\1", tag)
    file.exists(file.path(tmp, sub("^/", "", part)))
  }, logical(1))
  if (!all(keep)) {
    ct <- gsub("<Override[^>]*/>", "", ct)
    ct <- sub("</Types>", paste0(paste(tags[keep], collapse = ""), "</Types>"),
              ct, fixed = TRUE)
    writeLines(ct, ct_file, useBytes = TRUE)
  }
  invisible(TRUE)
}

#' Inject a native line chart into an openxlsx-generated workbook.
inject_line_chart <- function(xlsx_path, sheet_name, n_rows, series_cols,
                              series_names, colours, title) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("The zip package is required to inject the native chart.", call. = FALSE)
  }
  tmp <- tempfile("ms_chart_")
  dir.create(tmp, recursive = TRUE)
  utils::unzip(xlsx_path, exdir = tmp)

  wb_xml <- paste(readLines(file.path(tmp, "xl", "workbook.xml"),
                            warn = FALSE), collapse = "")
  pattern <- sprintf('<sheet[^>]*name="%s"[^>]*r:id="([^"]+)"',
                     gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", sheet_name))
  hit <- regmatches(wb_xml, regexec(pattern, wb_xml))[[1]]
  if (length(hit) < 2) {
    stop("Could not locate sheet ", sheet_name, " in workbook.xml.", call. = FALSE)
  }
  wb_rid <- hit[[2]]

  wb_rels <- paste(readLines(file.path(tmp, "xl", "_rels",
                                       "workbook.xml.rels"), warn = FALSE),
                   collapse = "")
  rel_pattern <- sprintf('<Relationship[^>]*Id="%s"[^>]*Target="([^"]+)"', wb_rid)
  rel_hit <- regmatches(wb_rels, regexec(rel_pattern, wb_rels))[[1]]
  if (length(rel_hit) < 2) {
    stop("Could not resolve the worksheet relationship for ", sheet_name,
         call. = FALSE)
  }
  target <- rel_hit[[2]]
  target <- sub("^/xl/", "", target)
  target <- sub("^\\.\\./", "", target)
  sheet_file <- file.path(tmp, "xl", target)
  if (!file.exists(sheet_file)) {
    stop("Worksheet part not found: ", sheet_file, call. = FALSE)
  }

  sheet_rels_dir <- file.path(dirname(sheet_file), "_rels")
  dir.create(sheet_rels_dir, showWarnings = FALSE, recursive = TRUE)
  sheet_rels <- file.path(sheet_rels_dir, paste0(basename(sheet_file), ".rels"))
  chart_part <- "xl/charts/chart1.xml"

  # openxlsx writes a drawing relationship (usually rId1) even when it has no
  # drawing part. Reuse that relationship and create the part it points at,
  # rather than adding a second relationship to the same worksheet.
  rels <- if (file.exists(sheet_rels)) {
    paste(readLines(sheet_rels, warn = FALSE), collapse = "")
  } else {
    ""
  }
  rel_tags <- if (nzchar(rels)) {
    regmatches(rels, gregexpr("<Relationship[^>]*/>", rels))[[1]]
  } else {
    character(0)
  }
  drawing_tags <- rel_tags[grepl("/drawing\"", rel_tags, fixed = TRUE)]
  if (length(drawing_tags) > 0) {
    drawing_id <- sub('.*Id="([^"]+)".*', "\\1", drawing_tags[[1]])
    drawing_target <- sub('.*Target="([^"]+)".*', "\\1", drawing_tags[[1]])
  } else {
    drawing_id <- "rId9001"
    drawing_target <- "../drawings/drawing1.xml"
  }
  drawing_part <- sub("^\\.\\./", "xl/", drawing_target)
  drawing_path <- file.path(tmp, drawing_part)
  dir.create(dirname(drawing_path), showWarnings = FALSE, recursive = TRUE)

  if (length(drawing_tags) == 0) {
    if (!nzchar(rels)) {
      rels <- rels_xml(
        drawing_id,
        "http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing",
        drawing_target
      )
    } else {
      rels <- sub(
        "</Relationships>",
        paste0('<Relationship Id="', drawing_id,
               '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing"',
               ' Target="', drawing_target, '"/></Relationships>'),
        rels, fixed = TRUE
      )
    }
    writeLines(rels, sheet_rels, useBytes = TRUE)
  }

  dir.create(file.path(tmp, "xl", "charts"), showWarnings = FALSE,
             recursive = TRUE)
  writeLines(chart_xml(sheet_name, n_rows, series_cols, series_names, colours,
                       title),
             file.path(tmp, chart_part), useBytes = TRUE)
  writeLines(drawing_xml("rId1"), drawing_path, useBytes = TRUE)
  drawing_rels_dir <- file.path(dirname(drawing_path), "_rels")
  dir.create(drawing_rels_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(
    rels_xml("rId1",
             "http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart",
             "../charts/chart1.xml"),
    file.path(drawing_rels_dir, paste0(basename(drawing_path), ".rels")),
    useBytes = TRUE
  )

  sheet_xml <- paste(readLines(sheet_file, warn = FALSE), collapse = "")
  if (!grepl("xmlns:r=", sheet_xml, fixed = TRUE)) {
    sheet_xml <- sub(
      "<worksheet ",
      paste0('<worksheet xmlns:r="http://schemas.openxmlformats.org/',
             'officeDocument/2006/relationships" '),
      sheet_xml, fixed = TRUE
    )
  }
  if (grepl("<drawing ", sheet_xml, fixed = TRUE)) {
    sheet_xml <- sub("<drawing[^>]*/>",
                     paste0('<drawing r:id="', drawing_id, '"/>'),
                     sheet_xml)
  } else {
    sheet_xml <- sub("</worksheet>",
                     paste0('<drawing r:id="', drawing_id, '"/></worksheet>'),
                     sheet_xml, fixed = TRUE)
  }
  writeLines(sheet_xml, sheet_file, useBytes = TRUE)

  ct_file <- file.path(tmp, "[Content_Types].xml")
  ct <- paste(readLines(ct_file, warn = FALSE), collapse = "")
  additions <- paste0(
    '<Override PartName="/xl/charts/chart1.xml" ',
    'ContentType="application/vnd.openxmlformats-officedocument.drawingml.chart+xml"/>',
    '<Override PartName="/', drawing_part, '" ',
    'ContentType="application/vnd.openxmlformats-officedocument.drawing+xml"/>'
  )
  ct <- sub("</Types>", paste0(additions, "</Types>"), ct, fixed = TRUE)
  writeLines(ct, ct_file, useBytes = TRUE)

  prune_missing_parts(tmp)

  files <- list.files(tmp, recursive = TRUE, all.files = TRUE,
                      include.dirs = FALSE, no.. = TRUE)
  out_tmp <- tempfile(fileext = ".xlsx")
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)
  zip::zipr(out_tmp, files = files, root = tmp, mode = "mirror")
  setwd(old)
  file.copy(out_tmp, xlsx_path, overwrite = TRUE)
  unlink(c(tmp, out_tmp), recursive = TRUE)
  invisible(xlsx_path)
}

flatten_params <- function(x, prefix = "") {
  if (is.list(x)) {
    return(unlist(lapply(names(x), function(nm) {
      flatten_params(x[[nm]], if (nzchar(prefix)) paste0(prefix, ".", nm) else nm)
    })))
  }
  if (length(x) > 1) {
    return(stats::setNames(paste(x, collapse = ", "), prefix))
  }
  stats::setNames(as.character(x), prefix)
}

#' Build excel/MacroSignals_Desk.xlsx from a completed research run.
build_workbook <- function(results_dir = "results",
                           out_path = "excel/MacroSignals_Desk.xlsx",
                           db_path = "data/macrosignals.sqlite",
                           params_path = "config/params.yml") {
  metrics <- readr::read_csv(file.path(results_dir, "metrics.csv"),
                             show_col_types = FALSE)
  curves <- readr::read_csv(file.path(results_dir, "equity_curve.csv"),
                            show_col_types = FALSE)
  ranked <- readr::read_csv(file.path(results_dir, "ranked_latest.csv"),
                            show_col_types = FALSE)
  sens <- readr::read_csv(file.path(results_dir, "cost_sensitivity.csv"),
                          show_col_types = FALSE)
  val_path <- file.path(results_dir, "validation_log.csv")
  validation <- if (file.exists(val_path)) {
    readr::read_csv(val_path, show_col_types = FALSE)
  } else {
    data.frame(run_id = character(0), checked_at = character(0),
               series_id = character(0), check_name = character(0),
               severity = character(0), message = character(0))
  }
  params <- load_params(params_path)
  hash <- params_hash(params_path)
  run_id <- if (nrow(metrics) > 0) metrics$run_id[[1]] else "unknown"
  as_of <- if (nrow(ranked) > 0) as.character(ranked$as_of_date[[1]]) else
    as.character(params$as_of)
  fails <- if (nrow(validation) > 0) sum(validation$severity == "fail") else 0

  net <- metrics[metrics$basis == "net", ]
  gross <- metrics[metrics$basis == "gross", ]

  wb <- openxlsx::createWorkbook(creator = "MacroSignals")
  sheets <- c("Dashboard", "Ranked_Signals", "Metrics", "EquityCurve",
              "Validation", "Params")
  for (s in sheets) openxlsx::addWorksheet(wb, s)

  # --- Dashboard -----------------------------------------------------------
  title_style <- openxlsx::createStyle(fontSize = 18, textDecoration = "bold",
                                       fontColour = "#1F3864")
  label_style <- openxlsx::createStyle(textDecoration = "bold",
                                       fontColour = "#44546A")
  openxlsx::writeData(wb, "Dashboard", "MacroSignals — Desk Dashboard",
                      startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, "Dashboard", title_style, rows = 1, cols = 1)
  labels <- c("Run id", "As of", "Last refresh", "Params MD5",
              "Data-quality status")
  values <- c(run_id, as_of, "workbook build", hash,
              if (fails == 0) "PASS" else paste0("FAIL (", fails, ")"))
  for (i in seq_along(labels)) {
    openxlsx::writeData(wb, "Dashboard", labels[[i]], startRow = 2 + i,
                        startCol = 1)
    openxlsx::writeData(wb, "Dashboard", values[[i]], startRow = 2 + i,
                        startCol = 2)
    openxlsx::addStyle(wb, "Dashboard", label_style, rows = 2 + i, cols = 1)
  }
  status_row <- 2 + length(labels)  # B7
  status_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center",
    fgFill = if (fails == 0) "#C6EFCE" else "#FFC7CE",
    fontColour = if (fails == 0) "#006100" else "#9C0006"
  )
  openxlsx::addStyle(wb, "Dashboard", status_style, rows = status_row,
                     cols = 2, stack = TRUE)
  openxlsx::writeData(wb, "Dashboard",
                      paste0("Headline metrics — net of ", params$cost_bp, " bp"),
                      startRow = status_row + 2, startCol = 1)
  openxlsx::addStyle(wb, "Dashboard", label_style, rows = status_row + 2,
                     cols = 1)

  dash_cols <- c("signal_name", "ann_return", "ann_vol", "sharpe", "max_dd",
                 "calmar", "ann_turnover", "avg_holding_days")
  dash <- net[, dash_cols]
  names(dash) <- c("Signal", "Ann. return", "Ann. vol", "Sharpe", "Max DD",
                   "Calmar", "Turnover", "Holding days")
  openxlsx::writeData(wb, "Dashboard", dash, startRow = status_row + 3,
                      startCol = 1, headerStyle = openxlsx::createStyle(
                        textDecoration = "bold", fgFill = "#D9E1F2",
                        border = "bottom"
                      ))
  pct_style <- openxlsx::createStyle(numFmt = "0.00%")
  num_style <- openxlsx::createStyle(numFmt = "0.00")
  openxlsx::addStyle(wb, "Dashboard", pct_style,
                     rows = (status_row + 4):(status_row + 3 + nrow(dash)),
                     cols = c(2, 3, 5), gridExpand = TRUE)
  openxlsx::addStyle(wb, "Dashboard", num_style,
                     rows = (status_row + 4):(status_row + 3 + nrow(dash)),
                     cols = c(4, 6, 7, 8), gridExpand = TRUE)
  openxlsx::writeData(
    wb, "Dashboard",
    paste("Status light is driven by the Validation sheet;",
          "macro RefreshSignals refuses to run while any FAIL row exists."),
    startRow = status_row + 6 + nrow(dash), startCol = 1
  )
  openxlsx::setColWidths(wb, "Dashboard", cols = 1:8,
                         widths = c(18, 22, 18, 14, 12, 12, 12, 14))

  # --- Ranked signals ------------------------------------------------------
  ranked_out <- ranked[, c("rank", "instrument_id", "name", "asset_class",
                           "carry_z", "momentum_z", "macro_surprise_z",
                           "composite_z")]
  openxlsx::writeDataTable(
    wb, "Ranked_Signals", ranked_out, startRow = 1, startCol = 1,
    tableStyle = "TableStyleMedium2", tableName = "tblRanked",
    withFilter = TRUE
  )
  openxlsx::conditionalFormatting(
    wb, "Ranked_Signals", cols = 5:8, rows = 2:(nrow(ranked_out) + 1),
    style = c("#F8696B", "#FFEB84", "#63BE7B"), type = "colourScale"
  )
  openxlsx::addStyle(wb, "Ranked_Signals", num_style,
                     rows = 2:(nrow(ranked_out) + 1), cols = 5:8,
                     gridExpand = TRUE)
  openxlsx::freezePane(wb, "Ranked_Signals", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, "Ranked_Signals", cols = 1:8,
                         widths = c(7, 14, 34, 12, 10, 11, 14, 12))

  # --- Metrics -------------------------------------------------------------
  metric_cols <- c("run_id", "signal_name", "basis", "ann_return", "ann_vol",
                   "sharpe", "max_dd", "dd_start", "dd_end", "calmar",
                   "hit_rate", "ann_turnover", "avg_holding_days", "n_days",
                   "cost_bp")
  m_out <- metrics[, metric_cols]
  openxlsx::writeData(wb, "Metrics", m_out, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, "Metrics", openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom"
  ), rows = 1, cols = seq_along(metric_cols))
  openxlsx::addStyle(wb, "Metrics", pct_style,
                     rows = 2:(nrow(m_out) + 1), cols = c(4, 5, 7, 11),
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, "Metrics", num_style,
                     rows = 2:(nrow(m_out) + 1), cols = c(6, 10, 12, 13),
                     gridExpand = TRUE)
  openxlsx::freezePane(wb, "Metrics", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, "Metrics", cols = seq_along(metric_cols),
                         widths = c(30, 14, 8, 11, 10, 9, 10, 12, 12, 9, 9,
                                    12, 13, 8, 8))

  # --- Equity curve --------------------------------------------------------
  wide <- tidyr::pivot_wider(
    curves[, c("date", "signal_name", "cum_net")],
    id_cols = "date", names_from = "signal_name", values_from = "cum_net"
  )
  wide <- wide[order(wide$date), ]
  series_order <- intersect(c("carry", "momentum", "macro_surprise", "composite"),
                            names(wide))
  wide <- wide[, c("date", series_order)]
  openxlsx::writeData(wb, "EquityCurve", wide, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, "EquityCurve", openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom"
  ), rows = 1, cols = seq_len(ncol(wide)))
  openxlsx::addStyle(wb, "EquityCurve",
                     openxlsx::createStyle(numFmt = "yyyy-mm-dd"),
                     rows = 2:(nrow(wide) + 1), cols = 1)
  openxlsx::addStyle(wb, "EquityCurve", num_style,
                     rows = 2:(nrow(wide) + 1), cols = 2:ncol(wide),
                     gridExpand = TRUE)
  openxlsx::freezePane(wb, "EquityCurve", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, "EquityCurve", cols = seq_len(ncol(wide)),
                         widths = c(12, 12, 12, 14, 12))

  # --- Validation ----------------------------------------------------------
  val_cols <- c("run_id", "checked_at", "series_id", "check_name", "severity",
                "message")
  if (nrow(validation) == 0) {
    validation <- data.frame(run_id = character(0), checked_at = character(0),
                             series_id = character(0), check_name = character(0),
                             severity = character(0), message = character(0))
  }
  openxlsx::writeData(wb, "Validation", validation[, val_cols],
                      startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, "Validation", openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom"
  ), rows = 1, cols = seq_along(val_cols))
  openxlsx::freezePane(wb, "Validation", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, "Validation", cols = seq_along(val_cols),
                         widths = c(30, 20, 14, 18, 10, 70))
  if (nrow(validation) > 0) {
    fail_rows <- which(tolower(validation$severity) == "fail") + 1
    if (length(fail_rows) > 0) {
      openxlsx::addStyle(wb, "Validation", openxlsx::createStyle(
        fgFill = "#FFC7CE", fontColour = "#9C0006"
      ), rows = fail_rows, cols = seq_along(val_cols), gridExpand = TRUE,
      stack = TRUE)
    }
  }

  # --- Params --------------------------------------------------------------
  flat <- flatten_params(params)
  params_df <- data.frame(parameter = names(flat), value = unname(flat),
                          stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Params", params_df, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, "Params", openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom"
  ), rows = 1, cols = 1:2)
  openxlsx::freezePane(wb, "Params", firstRow = TRUE)
  openxlsx::setColWidths(wb, "Params", cols = 1:2, widths = c(42, 60))

  # --- Named ranges --------------------------------------------------------
  openxlsx::createNamedRegion(wb, "Ranked_Signals", name = "rngRanked",
                              rows = 1:(nrow(ranked_out) + 1), cols = 1:8)
  openxlsx::createNamedRegion(wb, "Metrics", name = "rngMetrics",
                              rows = 1:(nrow(m_out) + 1),
                              cols = seq_along(metric_cols))
  openxlsx::createNamedRegion(wb, "Dashboard", name = "rngStatus",
                              rows = status_row, cols = 2)

  ensure_dir(dirname(out_path))
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

  # openxlsx has no chart API: add the native line chart as an OOXML part.
  inject_line_chart(
    out_path,
    sheet_name = "EquityCurve",
    n_rows = nrow(wide),
    series_cols = 2:ncol(wide),
    series_names = names(wide)[-1],
    colours = c("#2E86AB", "#E67E22", "#27AE60", "#8E44AD"),
    title = "Net equity curves"
  )
  invisible(out_path)
}
