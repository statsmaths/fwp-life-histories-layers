library(tidyverse)
library(stringi)
library(xml2)

# cp layers/*.js ~/gh/writing-their-voices/src/components/layers/

# pandoc -t tei -s -i docx/00-intro.docx -o tei/00-intro.xml
# pandoc -t tei -s -i docx/01-layer1.docx -o tei/01-layer1.xml
# pandoc -t tei -s -i docx/02-layer2.docx -o tei/02-layer2.xml
# pandoc -t tei -s -i docx/03-layer3.docx -o tei/03-layer3.xml
# pandoc -t tei -s -i docx/04-layer4.docx -o tei/04-layer4.xml
# pandoc -t tei -s -i docx/05-conclusion.docx -o tei/05-conclusion.xml
# pandoc -t tei -s -i docx/06-methods.docx -o tei/06-methods.xml
# pandoc -t tei -s -i docx/07-bibliography.docx -o tei/07-bibliography.xml

# <div className="text-holder">
# <Collapsible trigger="Section Name" overflowWhenOpen="visible">
# <sup className="tooltip">[2]<span>TEXT HERE</span></sup>

template <- "
import React from 'react';

%s

function Layer%d(props) {

    return(
      %s
    )
}


export {Layer%s};
"
template_col <- "import Collapsible from 'react-collapsible';"

to_process <- stri_sub(dir("tei", pattern = ".xml$"), 1L, -5L)
for (fp in to_process)
{
  print(fp)
  num <- as.numeric(stri_sub(fp, 1, 2)) + 1L
  x <- read_xml(file.path("tei", paste0(fp, ".xml")))

  head_node <- xml_find_all(x, "//d1:body/d1:head")
  xml_name(head_node) <- "h1"

  hi_nodes <- xml_find_all(x, "//d1:hi")
  xml_name(hi_nodes) <- "em"
  xml_set_attr(hi_nodes, attr = "rendition", value = NULL)

  notes <- xml_find_all(x, "//d1:note")
  xml_name(notes) <- "sup"
  xml_set_attr(notes, attr = "className", value = "tooltip")

  for (i in seq_along(notes))
  {
    p <- xml_find_all(notes[i], "./d1:p")
    xml_name(p) <- "span"
    new_node <- xml_new_root("div")
    xml_set_namespace(new_node, "d1", "http://www.tei-c.org/ns/1.0")
    xml_set_text(new_node, sprintf("[%d]", i))
    xml_add_sibling(p, new_node, where = "before")
  }

  collapse <- xml_find_all(x, "//d1:collapse")
  xml_name(collapse) <- "Collapsible"
  xml_set_attr(collapse, attr = "overflowWhenOpen", value = "visible")

  body <- xml_find_first(x, ".//d1:body")
  xml_name(body) <- "div"
  xml_set_attr(body, attr = "className", value = "text-holder")

  quote <- xml_find_first(x, ".//d1:quote")
  xml_name(quote) <- "div"
  xml_set_attr(quote, attr = "className", value = "emquote")


  write_html(body, file.path("html", paste0(fp, ".html")))

  txt <- readLines(file.path("html", paste0(fp, ".html")), warn = FALSE)
  txt <- paste(txt, collapse = " ")
  txt <- stri_replace_all(txt, " ", fixed = "\t")
  txt <- stri_replace_all(txt, " ", fixed = "\n")
  txt <- stri_replace_all(txt, " ", regex = "[ ]+")

  tem_inner <- if (stri_detect(txt, fixed = "Collapsible")) {
    template_col
  } else { "" }
  output <- sprintf(template, tem_inner, num, txt, num)
  write_lines(output, file.path("layers", sprintf("Layer%d.js", num)))
}
