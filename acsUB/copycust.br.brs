00010   open #1: "Name=Ubmstr.M\Customer.h1,KFName=Ubmstr.M\ubIndex.h1",internal,input,keyed 
00020   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h1,KFName="&env$('Q')&"\UBmstr\ubindex.h1",internal,outIn,keyed 
00030   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7,gb(10),unused$*1274,df$*1,dr$*9,dc$*2,da$*17,extra(23),extra$(11)*30
00040 L40: form c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,c 1274,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 9,n 1,2*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00050   read #1,using L40,key=" 801027.00": z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,unused$,df$,dr$,dc$,da$,mat extra,mat extra$
00060   rewrite #2,using L40,key=" 801027.00": z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,unused$,df$,dr$,dc$,da$,mat extra,mat extra$
00070   close #1: 
00080   close #2: 
