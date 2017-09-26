00010   dim line$*256,acct$(1),other_charge(1),x(15)
00020   open #1: 'Name=C:\Users\John\Desktop\april_1_billing.csv',display,input 
00030   linput #1: line$ ! consume headings
00040   do 
00050     linput #1: line$ eof CSV_EOF
00060     let line_count+=1
00070     mat acct$(line_count)
00080     mat other_charge(line_count)
00090     let acct$(line_count)=line$(1:pos(line$,',')-1)
00100     if pos(acct$(line_count),'.')<=0 then let acct$(line_count)=trim$(acct$(line_count))&'.00'
00110     let acct$(line_count)=trim$(acct$(line_count))
00120     let other_charge(line_count)=val(line$(pos(line$,',')+1:len(line$)))
00130   loop 
00140 CSV_EOF: ! 
00150   close #1: 
00160 ! open #2: 'Name=z:\acs\acs.402\ubmstr\iphold9.h440',internal,outin
00170   open #2: 'Name=C:\Users\John\Desktop\iphold9.h1',internal,outin 
00180   do 
00190     read #2,using 'form pos 1,cr 10,4*pd 5,7*pd 4.2,3*pd 5,n 1': x$,mat x eof IPHHOLD_EOF
00200     let which=srch(mat acct$,trim$(x$))
00210     if which>0 then 
00220       let x(8)+=other_charge(which)
00230     end if 
00240     rewrite #2,using 'form pos 1,cr 10,4*pd 5,7*pd 4.2,3*pd 5,n 1': x$,mat x
00250   loop 
00260 IPHHOLD_EOF: ! 
00270   end 
