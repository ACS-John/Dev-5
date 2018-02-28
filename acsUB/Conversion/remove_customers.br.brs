00010 ! replace S:\acsUB\conversion\remove_customers
00020 ! programmers tool to remove Customers (not for use on real data)
00030 ! _______________
00040 ! library
00050 ! on error goto ertn
00060 ! ____________
00070   cno=2
00080   open #11: "Name=[Q]\UBmstr\Customer.h[cno]",internal,outIn 
00090 L90: read #11,using "Form pos 1,C 10": p$ eof L120
00100   if p$(1:2)=" 6" then delete #11: : pr "deleted one"
00110   goto L90
00120 L120: pr 'done'
