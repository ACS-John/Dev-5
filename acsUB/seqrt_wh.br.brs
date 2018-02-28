00010 !  REPLACE seqrt_wh.br
00020 ! Custom program to trim last two digits off 50-digit sequence numbers and set correct route numbers
00030   dim z$*10
00040 CUSTFORM: form c 10,pos 1741,n 2  ! ,n 7
00050   open #1: "Name=[Q]\UBmstr\Customer.h1,KFName=[Q]\UBmstr\UBIndex.h1",internal,outIn,keyed 
00060   do 
00070     read #1,using CUSTFORM: z$,rt eof XIT ! ,seq EOF XIT
00080 ! IF seq>9999 THEN seq=INT(seq/100)
00090     rt=val(trim$(z$(1:2)))
00100     rewrite #1,using CUSTFORM: z$,rt ! ,seq
00110   loop 
00120 XIT: close #1: 
00130   chain "menu"
