! rep test_keys
! verify that keys are being changed on rec= rewrites

open #1: "Name=xxx,KFName=xxx.ky1,replace,RecL=80,kps=1,kln=4",internal,outIn,keyed 
open #2: "Name=xxx,KFName=xxx.ky2,replace,RecL=80,kps=11,kln=4",internal,outIn,keyed 

FORM1: form n 4,x 6,n 4

for x=1 to 20
	write #1,using FORM1: x ! ,X+50
	asdf=rec(1)
	rewrite #1,using "form pos 11,N 4",rec=asdf: x+50
next x

close #1: : close #2: 

open #1: "Name=xxx,KFName=xxx.ky1",internal,outIn,keyed 
open #2: "Name=xxx,KFName=xxx.ky2",internal,outIn,keyed 
read #1,using FORM1,rec=17: a,b
pr a,b
rewrite #1,using FORM1,rec=17: 27,37
read #2,using FORM1,key="  37": a,b
pr a,b
