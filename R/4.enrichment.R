# The enrichment analysis is so widely used in omics data analysis, so I wrapped a few tools here to facilitate enrichment analysis.
gene <- up_genes

# msigdbr (https://pubmed.ncbi.nlm.nih.gov/26771021/)
# [todo] TissueEnrich (https://www.bioconductor.org/packages/release/bioc/vignettes/TissueEnrich/inst/doc/TissueEnrich.html#teenrichment-tissue-specific-gene-enrichment-using-human-or-mouse-genes)


require(msigdbr);


# clusterProfiler (https://pubmed.ncbi.nlm.nih.gov/34557778/)
# https://yulab-smu.top/biomedical-knowledge-mining-book/enrichment-overview.html
require(clusterProfiler); require(enrichplot); require(GOSemSim)
# ORA
ORA_GO <- function(gene, simplify = T){
  ego <- enrichGO(gene,
                  OrgDb    = "org.Hs.eg.db",
                  keyType  = "SYMBOL",
                  ont      = "ALL", # One of "BP", "MF", and "CC"
                  readable = TRUE)  # goplot(ego2, showCategory = 10)
  if (simplify) ego2 <- simplify(ego, cutoff=0.7)
}



geneList <- deg_results %>%
  tibble::rownames_to_column("gene") %>%
  arrange(dplyr::desc(avg_log2FC)) %>%
  select(gene, avg_log2FC) %>%
  format_geneList()
ego3 <- gseGO(geneList     = geneList,
              OrgDb        = "org.Hs.eg.db",
              keyType      = "SYMBOL",
              ont          = "ALL",
              minGSSize    = 100, # minimal size of each geneSet for analyzing
              maxGSSize    = 500, # maximal size of genes annotated for testing
              pvalueCutoff = 0.05,
              verbose      = FALSE)
ego4 <- simplify(ego3, cutoff=0.7)
emapplot(pairwise_termsim(ego3), cex_category=1.5)
gseaplot2(ego4, geneSetID = 'GO:0003823', pvalue_table=T, color="black")
gseaplot2(ego4, geneSetID = ego4@result$ID[1:5], pvalue_table=T)
geneInCategory(ego4)["GO:0003823"]
SeuratExtend::GSEAplot(
  pbmc, ident.1 = "CD4 T Naive", title = "INTERFERON_GAMMA_RESPONSE",
  geneset = hall50$human$HALLMARK_INTERFERON_GAMMA_RESPONSE)
SCP::RunGSEA()

#' Format gene list for GSEA
#'
#' You can download gmt file from https://maayanlab.cloud/Enrichr/#libraries
#' The tutorial for this func can refer to https://yulab-smu.top/biomedical-knowledge-mining-book/faq.html#genelist
#'
#' @param df A data frame with two column:
#'  *Column 1*: 1st column is **Gene ID**
#'  *Column 2*: 2nd column is **Gene Statistics** (e.g., fold change or other type of numerical variable, etc.)
#' @return A named vector of gene statistics, sorted in descending order.
#' @export
format_geneList <- function(df) {
  message("Formatting gene list for GSEA ...")
  geneList = df[,2]
  names(geneList) = as.character(df[,1])
  geneList = sort(geneList, decreasing = TRUE)
  return(geneList)
}


#' Format gene string to vector
#'
#' @param x A character vector or a list of character vectors, where each element may contain gene identifiers separated by a specified separator.
#' @param sep A character string that specifies the separator used to split the gene identifiers (default is "/").
#' @param unique_only A logical value indicating whether to return only unique gene identifiers (default is TRUE).
#' @return A list of character vectors, where each vector contains the unique gene identifiers from the corresponding element of `x`.
#' @note
#' I find this is equivalent to clusterProfiler::geneInCategory(ego4)["GO:0003823"]...
#' @examples
#' # Example usage:
#' ego4@result$core_enrichment[1] # ""IGLV3-19/IGHM/IGLV7-43/HLA-DPA1/IGHV3-7"
#' ego4@result$core_enrichment[1] %>% format_gene_str2vec() # c("IGLV3-19", "IGHM", "IGLV7-43", "HLA-DPA1", "IGHV3-7")
#' deg_results %>% filter(rownames(.) %in% format_gene_str2vec(ego4@result$core_enrichment[1])) %>% View()
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
