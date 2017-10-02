00010   dim a$(3)
00020   pr newpage
00030   a$(1)='abc'
00040   a$(2)='def'
00050   a$(3)='ghi'
00060   pr fields "2,2,combo 10,+,select": mat a$
00070   input fields "2,2,combo 10": d$
