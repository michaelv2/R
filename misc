getPackageVer <- function(package.name) {
  manifest <- installed.packages()[,c('Package','Version')]
  manifest[rownames(manifest) %in% package.name,]
}
