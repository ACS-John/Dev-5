00010   dim a$(3)
00020   print newpage
00030   let a$(1)='abc'
00040   let a$(2)='def'
00050   let a$(3)='ghi'
00060   print fields "2,2,combo 10,+,select": mat a$
00070   input fields "2,2,combo 10": d$
