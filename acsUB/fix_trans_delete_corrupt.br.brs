autoLibrary
open #h_trans:=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr",internal,outIn,keyed 
err_count=0
do 
	dim a$*10,tg(10)
	read #h_trans,using F_TRANS: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode eof Finis ioerr ErrorOnIt
	F_TRANS: form c 10,n 8,n 1,12*pd 4.2,2*pd 5,pos 98,pd 4.2,n 1
loop 
ErrorOnIt: ! 
	err_count+=1
	pr rec(h_trans)
	delete #h_trans: 
continue 
Finis: ! 
	pr 'total errors encountered: ';err_count
end 
