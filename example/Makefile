EMACS_SESSION ?= org-lecture-example

# ORG_SLIDES: Which ORG files should be tangled into .tex files and
# translated into PDF files.
ORG_SLIDES=01-einleitung.org

# ORG_HTML: Which ORG files are translated, without PDF processing,
# into html files.
ORG_HTML=index.org

REMOTE=/tmp/remote

OL_DIR=..
include ../common.mk

$(eval $(call PROCESS_STAMP,01,fig/01-picture.pdf,0))

# Generate Picture for HTML files from PDF graphic
$(eval $(call PROCESS_PDF2PNG,fig/01-picture.pdf,1))
01.html: fig/01-picture-1.png

# Special cases for the index file
build/html/index.html: $(ORG_HTML) $(ORG_SLIDES)
