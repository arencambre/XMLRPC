test_that("rpc.serialize works", {
  o = list(list(a = 1, b = 2), list(abc = "xyz", de = TRUE))
  xml = rpc.serialize(o)
  txt = saveXML(xml)
  
  expect_equal(
    txt,
    "<array>\n  <data>\n    <value>\n      <struct>\n        <member>\n          <name>a</name>\n          <value>\n            <double>1</double>\n          </value>\n        </member>\n        <member>\n          <name>b</name>\n          <value>\n            <double>2</double>\n          </value>\n        </member>\n      </struct>\n    </value>\n    <value>\n      <struct>\n        <member>\n          <name>abc</name>\n          <value>\n            <string><![CDATA[xyz]]></string>\n          </value>\n        </member>\n        <member>\n          <name>de</name>\n          <value>\n            <boolean>1</boolean>\n          </value>\n        </member>\n      </struct>\n    </value>\n  </data>\n</array>"
  )
  
  expect_equal(saveXML(vectorArray(1:10, "int")), saveXML(XMLRPC:::vectorArray(1:10, "int")))
  
  expect_equal(saveXML(vectorArray(1:10, "i4")), saveXML(XMLRPC:::vectorArray(1:10, "i4")))
  
  expect_equal(saveXML(vectorArray(1:10, "string")), saveXML(XMLRPC:::vectorArray(1:10, "string")))
  
  expect_equal(saveXML(vectorArray(1:10, "boolean")), saveXML(XMLRPC:::vectorArray(1:10, "boolean")))
  
  expect_equal(saveXML(vectorArray(c(TRUE, FALSE), "boolean")),
               saveXML(XMLRPC:::vectorArray(c(TRUE, FALSE), "boolean")))
  
  
  #########
  d = c(Sys.Date(), Sys.Date() - 1)
  expect_equal(saveXML(vectorArray(d, "dateTime.iso8601")),
               saveXML(XMLRPC:::vectorArray(d, "dateTime.iso8601")))
  
  
  d = c(Sys.time(), Sys.time() - 1)
  expect_equal(saveXML(vectorArray(d, "dateTime.iso8601")),
               saveXML(XMLRPC:::vectorArray(d, "dateTime.iso8601")))
  
  expect_equal(saveXML(vectorArray(c(0, 1, 2, 3), "double")),
               saveXML(XMLRPC:::vectorArray(c(0, 1, 2, 3), "double")))
  
  
  # types
  
  doc = xmlParse('../xmlrpcTypes.xml')
  top = xmlRoot(doc)
  
  expect_equal(
    XMLRPC:::xmlRPCToR(top[["array"]]),
    list(
      value = 12L,
      value = "Egypt",
      value = FALSE,
      value = -31L
    )
  )
  expect_equal(XMLRPC:::xmlRPCToR(top[["struct"]]),
               list(lowerBound = 18L, upperBound = 139L))
  expect_equal(rawToChar(XMLRPC:::xmlRPCToR(top[["base64"]])), "you can't read this!")
  
  expect_equal(
    xmlApply(top, XMLRPC:::xmlRPCToR),
    list(
      int = 10L,
      i4 = 11L,
      string = "A string",
      double = 3.1415,
      boolean = TRUE,
      boolean = FALSE,
      dateTime.iso8601 = structure(
        900648535,
        class = c("POSIXct",
                  
                  "POSIXt"),
        tzone = ""
      ),
      base64 = charToRaw("you can't read this!"),
      array = list(
        value = 12L,
        value = "Egypt",
        value = FALSE,
        value = -31L
      ),
      struct = list(lowerBound = 18L, upperBound = 139L)
    )
  )
  
  # Round trip the data.
  x = XMLRPC:::rpc.serialize(1:10)
  expect_equal(XMLRPC:::xmlRPCToR(x),1:10)
})
