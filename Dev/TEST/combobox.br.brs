dim a$(3)
pr newpage
a$(1)='abc'
a$(2)='def'
a$(3)='ghi'
pr f "2,2,combo 10,+,select": mat a$
input fields "2,2,combo 10": d$
