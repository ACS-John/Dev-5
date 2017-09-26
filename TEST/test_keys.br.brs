00010 ! rep test_keys
00020 ! verify that keys are being changed on rec= rewrites
00030 ! 
00040   open #1: "Name=xxx,KFName=xxx.ky1,replace,RecL=80,kps=1,kln=4",internal,outin,keyed 
00050   open #2: "Name=xxx,KFName=xxx.ky2,replace,RecL=80,kps=11,kln=4",internal,outin,keyed 
00060 ! 
00070 FORM1: form n 4,x 6,n 4
00080 ! 
00090   for x=1 to 20
00100     write #1,using FORM1: x ! ,X+50
00101     let asdf=rec(1)
00102     rewrite #1,using "Form Pos 11,N 4",rec=asdf: x+50
00110   next x
00120 ! 
00130   close #1: : close #2: 
00140 ! 
00150   open #1: "Name=xxx,KFName=xxx.ky1",internal,outin,keyed 
00160   open #2: "Name=xxx,KFName=xxx.ky2",internal,outin,keyed 
00170   read #1,using FORM1,rec=17: a,b
00180   print a,b
00190   rewrite #1,using FORM1,rec=17: 27,37
00200   read #2,using FORM1,key="  37": a,b
00220   print a,b
