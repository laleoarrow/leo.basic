# ======== enrichment helper ================
#' Enrichment helpers
#' @name enrichment_helper
#' @title Enrichment helpers
#' @description Utilities to format gene lists and collapse IDs for KEGG/Reactome GSEA/ORA.
#' * You can download gmt file from https://maayanlab.cloud/Enrichr/#libraries
#' * The tutorial for this func can refer to https://yulab-smu.top/biomedical-knowledge-mining-book/faq.html#genelist
#' @section Functions:
#' - \code{format_geneList}: make a named numeric vector sorted decreasingly
#' - \code{format_gene_str2vec}: split core_enrichment string into gene vector
#' - \code{prep_GSEA_geneList}: map names to ENTREZID and keep 1 score/gene by |score| max
#' @seealso clusterProfiler, ReactomePA
NULL

#' @rdname enrichment_helper
#' @description Build a named numeric vector sorted decreasingly
#' @param df A data frame with two column:
#'  *Column 1*: 1st column is **Gene ID**
#'  *Column 2*: 2nd column is **Gene Statistics** (e.g., fold change or other type of numerical variable, etc.)
#' @return A named numeric vector (descending)
#' @export
format_geneList <- function(df) {
  geneList = df[,2]
  names(geneList) = as.character(df[,1])
  geneList = sort(geneList, decreasing = TRUE)
  return(geneList)
}

#' @rdname enrichment_helper
#' @description Format enrichment gene string like "A/B/C" to vector
#' @param x A character vector or a list of character vectors
#' @param sep Separator, default "/"
#' @param unique_only Return unique identifiers, default TRUE
#' @return A character vector or a list of character vectors
#' @examples
#' # ego4@result$core_enrichment[1] -> "IGLV3-19/IGHM/IGLV7-43/HLA-DPA1/IGHV3-7"
#' # format_gene_str2vec(ego4@result$core_enrichment[1])
#' @export
format_gene_str2vec <- function(x, sep = "/", unique_only = TRUE) {
  if (length(x) == 0 || all(is.na(x))) return(character(0))
  x <- as.character(x)
  split_clean <- function(s) {
    v <- strsplit(s, split = sep, fixed = TRUE)[[1]]
    v <- trimws(v)
    v <- v[nzchar(v)]
    if (unique_only) v <- v[!duplicated(v)]
    v
  }
  res <- lapply(x, split_clean)
  if (length(res) == 1) return(res[[1]])
  names(res) <- if (!is.null(names(x))) names(x) else as.character(seq_along(x))
  return(res)
}

#' @rdname enrichment_helper
#' @description Map SYMBOL-named vector to ENTREZID and keep 1 score/gene by slice_max(|score|)
#' @param geneList_sym Named numeric vector; names are SYMBOL; values are scores
#' @return Named numeric vector whose names are unique ENTREZID (descending)
#' @examples
#' \dontrun{
#' data(geneList, package = "DOSE")
#' gl_sym <- stats::setNames(as.numeric(geneList), names(geneList)) # demo only
#' gl_entrez <- prep_GSEA_geneList(gl_sym)
#' }
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr inner_join group_by slice_max ungroup arrange desc
#' @importFrom clusterProfiler bitr
#' @importFrom org.Hs.eg.db org.Hs.eg.db
prep_GSEA_geneList <- function(geneList_sym) {
  stopifnot(is.numeric(geneList_sym), !is.null(names(geneList_sym)))
  map <- clusterProfiler::bitr(names(geneList_sym), fromType = "SYMBOL", toType = "ENTREZID",
                               OrgDb = org.Hs.eg.db::org.Hs.eg.db)
  df <- dplyr::tibble(SYMBOL = names(geneList_sym), score = as.numeric(geneList_sym)) %>%
    dplyr::inner_join(map, by = "SYMBOL") %>%
    dplyr::group_by(ENTREZID) %>%
    dplyr::slice_max(order_by = abs(score), n = 1, with_ties = FALSE, na_rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(score))
  gl <- df$score; names(gl) <- df$ENTREZID
  return(gl)
}


# ======== enrichment individual ============
#' Enrichment wrappers (individual)
#' @name enrichment_individual
#' @title Enrichment wrappers (individual)
#' @description ORA/GSEA wrappers for GO, KEGG, KEGG Module, and Reactome.
#' @section Functions:
#' - \code{ORA_GO}, \code{GSEA_GO}
#' - \code{ORA_KEGG}, \code{GSEA_KEGG}
#' - \code{ORA_MKEGG}, \code{GSEA_MKEGG}
#' - \code{ORA_Reactome}, \code{GSEA_Reactome}
#' @seealso leo_enrich
NULL

## GO -----
#' @rdname enrichment_individual
#' @param gene SYMBOL vector
#' @param simplify Use clusterProfiler::simplify for GO terms
#' @return enrichResult
#' @export
#' @importFrom clusterProfiler enrichGO simplify
#' @importFrom org.Hs.eg.db org.Hs.eg.db
ORA_GO <- function(gene, simplify = T) {
  ego <- clusterProfiler::enrichGO(gene = gene,
                                   OrgDb = org.Hs.eg.db,
                                   keyType = 'SYMBOL',
                                   ont = "ALL",
                                   pAdjustMethod = "BH",
                                   pvalueCutoff = 0.01,
                                   qvalueCutoff = 0.05,
                                   readable = TRUE)
  if (simplify) ego <- clusterProfiler::simplify(ego, cutoff = 0.7)
  return(ego)
}
#' @rdname enrichment_individual
#' @param geneList Named numeric vector (SYMBOL -> score)
#' @param simplify Use clusterProfiler::simplify for GO terms
#' @return gseaResult
#' @export
#' @importFrom clusterProfiler gseGO simplify
#' @importFrom org.Hs.eg.db org.Hs.eg.db
GSEA_GO <- function(geneList, simplify = T) {
  ego <- clusterProfiler::gseGO(geneList = geneList,
                                OrgDb = org.Hs.eg.db,
                                keyType = 'SYMBOL',
                                ont = "ALL",
                                pAdjustMethod = "BH",
                                minGSSize = 100,
                                maxGSSize = 500,
                                pvalueCutoff = 0.05,
                                verbose = T)
  if (simplify) ego <- clusterProfiler::simplify(ego, cutoff = 0.7)
  return(ego)
}

## KEGG -----
#' @rdname enrichment_individual
#' @param gene SYMBOL or ENTREZID vector depending on input
#' @param input "SYMBOL" or "ENTREZID"
#' @param organism KEGG organism code, default "hsa"
#' @return enrichResult
#' @export
#' @importFrom clusterProfiler enrichKEGG bitr
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @details
#' '''{R}
#' # KEGG_vis_notes: https://yulab-smu.top/biomedical-knowledge-mining-book/clusterprofiler-kegg.html
#' library("pathview")
#' kk <- gseKEGG(geneList     = geneList,
#'               keyType      = 'SYMBOL',
#'               organism     = 'hsa',
#'               minGSSize    = 120,
#'               pvalueCutoff = 0.05,
#'               verbose      = FALSE)
#' browseKEGG(kk, 'hsa04110')
#' # or
#' hsa04110 <- pathview(gene.data  = geneList,
#'                      pathway.id = "hsa04110",
#'                      species    = "hsa",
#'                      limit      = list(gene=max(abs(geneList)), cpd=1))
#' '''
ORA_KEGG <- function(gene, input = "SYMBOL") {
  if (input == "SYMBOL") {
    gene <- clusterProfiler::bitr(gene, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)$ENTREZID
    gene <- unique(gene)
  }
  kk <- clusterProfiler::enrichKEGG(gene = gene,
                                    organism = 'hsa',
                                    minGSSize = 10,
                                    pvalueCutoff = 0.05)
  return(kk)
}
#' @rdname enrichment_individual
#' @param geneList Named numeric vector (SYMBOL or ENTREZID per input)
#' @param input "SYMBOL" or "ENTREZID"
#' @return gseaResult
#' @export
#' @importFrom clusterProfiler gseKEGG
GSEA_KEGG <- function(geneList, input = "SYMBOL"){
  if (input == "SYMBOL") geneList <- prep_GSEA_geneList(geneList)
  kk <- gseKEGG(geneList = geneList,
                organism = 'hsa',
                minGSSize = 10,
                pvalueCutoff = 0.05,
                verbose = FALSE)
  return(kk)
}

## MKEGG pathway -----
#' @rdname enrichment_individual
#' @param gene SYMBOL or ENTREZID vector depending on input
#' @param input "SYMBOL" or "ENTREZID"
#' @return enrichResult
#' @export
#' @importFrom clusterProfiler enrichMKEGG bitr
#' @importFrom org.Hs.eg.db org.Hs.eg.db
ORA_MKEGG <- function(gene, input = "SYMBOL") {
  if (input == "SYMBOL") {
    gene <- clusterProfiler::bitr(gene, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)$ENTREZID
    gene <- unique(gene)
  }
  mkk <- clusterProfiler::enrichMKEGG(gene = gene,
                                      organism = 'hsa',
                                      pvalueCutoff = 1,
                                      qvalueCutoff = 1)
  return(mkk)
}
#' @rdname enrichment_individual
#' @param geneList Named numeric vector (SYMBOL or ENTREZID per input)
#' @param input "SYMBOL" or "ENTREZID"
#' @return gseaResult
#' @export
#' @importFrom clusterProfiler gseMKEGG
GSEA_MKEGG <- function(geneList, input = "SYMBOL") {
  if (input == "SYMBOL") geneList <- prep_GSEA_geneList(geneList)
  mkk <- clusterProfiler::gseMKEGG(geneList = geneList,
                                   organism = 'hsa',
                                   pvalueCutoff = 1)
  return(mkk)
}


## Reactome pathway -----
#' @rdname enrichment_individual
#' @param gene SYMBOL or ENTREZID vector depending on input
#' @param input "SYMBOL" or "ENTREZID"
#' @return enrichResult
#' @export
#' @importFrom ReactomePA enrichPathway
#' @importFrom clusterProfiler bitr
#' @importFrom org.Hs.eg.db org.Hs.eg.db
ORA_Reactome <- function(gene, input = "SYMBOL") {
  require(ReactomePA)
  if (input == "SYMBOL") {
    gene <- clusterProfiler::bitr(gene, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)$ENTREZID
    gene <- unique(gene)
  }
  reactome <- ReactomePA::enrichPathway(gene = gene,
                                        pvalueCutoff = 0.05,
                                        readable=TRUE)
  leo.basic::leo_log("Reactome vis: https://yulab-smu.top/biomedical-knowledge-mining-book/reactomepa.html")
  return(reactome)
}
#' @rdname enrichment_individual
#' @param geneList Named numeric vector (SYMBOL or ENTREZID per input)
#' @param input "SYMBOL" or "ENTREZID"
#' @return gseaResult
#' @export
#' @importFrom ReactomePA gsePathway
GSEA_Reactome <- function(geneList, input = "SYMBOL") {
  require(ReactomePA)
  if (input == "SYMBOL") geneList <- prep_GSEA_geneList(geneList)
  reactome <- ReactomePA::gsePathway(geneList = geneList,
                                     pvalueCutoff = 0.05,
                                     pAdjustMethod = "BH",
                                     verbose = FALSE)
  leo.basic::leo_log("Reactome vis: https://yulab-smu.top/biomedical-knowledge-mining-book/reactomepa.html")
  return(reactome)
}

# ======== MAIN ===========================
#' Enrichment analysis (Integrated)
#' @description Enrichment analysis for GO/KEGG/MKEGG/Reactome using ORA or GSEA; iterate method first, then background.
#' @param gene SYMBOL vector for ORA
#' @param geneList Named numeric vector for GSEA (names match input)
#' @param input "SYMBOL" or "ENTREZID" (for KEGG/MKEGG/Reactome)
#' @param simplify Whether to simplify GO results
#' @param method c("ORA","GSEA"), supports multiple
#' @param background c("GO","KEGG","MKEGG","Reactome"), supports multiple
#' @return A list of enrichment results per method-background combo
#' @examples
#' \dontrun{
#' #' @examples
#' # We here use SYMBOL in the first place
#' library(org.Hs.eg.db)
#' data(geneList, package="DOSE")
#' gene.df <- tibble(logFC=geneList, ID=names(geneList))
#' gene.df.map <- bitr(gene.df$ID, fromType = "ENTREZID", # optional
#'                     toType = c("SYMBOL"),
#'                     OrgDb = org.Hs.eg.db)
#' gene.df <- gene.df %>% left_join(gene.df.map, by = c("ID" = "ENTREZID")) %>% drop_na()
#' gene.df <- gene.df %>% arrange(desc(logFC))
#' gene <- gene.df$SYMBOL[1:100] # for ORA
#' geneList <- setNames(gene.df$logFC, gene.df$SYMBOL)  # for GSEA
#' }
#' @export
#' @importFrom clusterProfiler enrichGO gseGO enrichKEGG gseKEGG enrichMKEGG gseMKEGG bitr simplify
#' @importFrom ReactomePA enrichPathway gsePathway
#' @importFrom org.Hs.eg.db org.Hs.eg.db
leo_enrich <- function(gene, geneList, simplify =T, input = "SYMBOL",
                       method = c("ORA", "GSEA"),
                       background = c("GO", "KEGG", "MKEGG", "Reactome")) {
  require(org.Hs.eg.db); require(clusterProfiler)
  # match arguments
  method <- match.arg(method, several.ok = TRUE)
  background <- match.arg(background, several.ok = TRUE)
  # enrichement
  leo_log("A total of {length(method)} methods and {length(background)} backgrounds ({length(method) * length(background)} round{?s}) enrichement will be processed.")
  res_list <- list(); i = 0
  for (m in method) {
    for (bg in background) {
      i <- i + 1
      leo_log("Round {i}: Processing {m} for {bg} background...")
      res_list[[paste(m, bg, sep = "_")]] <- tryCatch(switch(
        m,
        ORA = switch(bg,
                     GO       = ORA_GO(gene, simplify),
                     KEGG     = ORA_KEGG(gene, input),
                     MKEGG    = ORA_MKEGG(gene, input),
                     Reactome = ORA_Reactome(gene, input)),
        GSEA = switch(bg,
                      GO       = GSEA_GO(geneList, simplify),
                      KEGG     = GSEA_KEGG(geneList, input),
                      MKEGG    = GSEA_MKEGG(geneList, input),
                      Reactome = GSEA_Reactome(geneList, input))
      ), error = function(e) { leo_log("Enrichment failed: {e$message}", level = "danger"); NULL })
    }
  }
  return(res_list)
}
