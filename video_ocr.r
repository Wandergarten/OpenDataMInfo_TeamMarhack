install.packages("tesseract")
library(tesseract)

#setwd("C:/Code/CollectoR/CollectoR/OCR_tesseract")

# English (default): PNG to Text
# tesseract engine Defaults to English ('eng')!
eng <- tesseract("eng")
textENpng <- ocr("./samples/sampleEN2.png", engine = eng)
cat(textENpng)
# write.table(textENpng, "./results/sampleEN2.txt")

# English (default): PDF to PNG to Text
eng <- tesseract("eng")
origENpdf <- pdftools::pdf_text("./samples/sampleEN1.pdf")[1]
pdf2pngfileEN <- pdftools::pdf_convert('./samples/sampleEN1.pdf', format = 'tiff', pages = 1, dpi = 800)
textENpdf <- tesseract::ocr(pdf2pngfileEN, engine = eng)
cat(textENpdf)
# write.table(textENpdf, "./results/sampleEN1.txt")

# German: PDF to PNG to Text (one page)
# Instructions to add tesseract language support
# tesseract_info() or tesseract_info()$available
# go to: https://github.com/tesseract-ocr/tessdata
# download correct language data pack from: https://github.com/tesseract-ocr/tessdata/blob/master/deu.traineddata?raw=true
# use curl; downloaded file has to be in here: C:\Users\chris\AppData\Local\tesseract4\tesseract4\tessdata: 
# curl::curl_download("https://github.com/tesseract-ocr/tessdata/blob/master/deu.traineddata?raw=true", "C:/Users/chris/AppData/Local/tesseract4/tesseract4/tessdata/deu.traineddata")
# (probably not necessary but y'know -- persistency when Win cache clears correct dir; put the data here, too: C:\Program Files\R\R-4.0.3\library\tesseract\tessdata)
# curl::curl_download("https://github.com/tesseract-ocr/tessdata/blob/master/deu.traineddata?raw=true", "C:/Program Files/R/R-4.0.3/library/tesseract/tessdata/deu.traineddata")
# full list of availables incl. meta: https://tesseract-ocr.github.io/tessdoc/Data-Files-in-different-versions.html
deu <- tesseract("deu")
origDEpdf1 <- pdftools::pdf_text("./samples/abstractDE.pdf")[1]
pdf2pngfileDE1 <- pdftools::pdf_convert('./samples/abstractDE.pdf', format = 'tiff', pages = 1, dpi = 800)
textDEpdf1 <- tesseract::ocr(pdf2pngfileDE1, engine = deu)
cat(textDEpdf1)
# write.table(textDEpdf1, "./results/abstractDE.txt")

# multi-page at once
deu <- tesseract("deu")
origDEpdfm <- pdftools::pdf_text("./samples/sampleDE.pdf")
pdf2pngfileDEm <- pdftools::pdf_convert('./samples/sampleDE.pdf', format = 'tiff', dpi = 800)
textDEpdfm <- tesseract::ocr(pdf2pngfileDEm, engine = deu)
cat(textDEpdfm)
# write.table(textDEpdfm, "./results/sampleDE.txt")
