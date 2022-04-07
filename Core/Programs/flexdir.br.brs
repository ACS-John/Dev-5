!  Replace S:\Core\FlexDir
! puts a flex grid with a directory inside... kinda a file explorer

autoLibrary
on error goto Ertn

dim resp$(10)*255
dim path$(2,1000)*255,prog$(2,1000)*100,ext$(2,1000)*100
dim brfn$(1000)*255,brsfn$(1000)*255,dur$*200,item$(1000)*255

fnTop('S:\Core\FlexDir','Flex Dir')
fngetcd(dur$)
REFREASH: !
	mat brfn$(1000) : mat brfn$=('')
	if filter$='' then filter$='*.*'
	fngetdir(dur$,mat brfn$,' /s ',filter$)
	for j=1 to udim(brfn$)
		if brfn$(j)='' then 
			mat brfn$(j)
			goto OUT_THE_LOOP
		else
			fnremove2(dur$,brfn$(j))
		end if
		fnGetPp(brfn$(j),path$(1,j),prog$(1,j),ext$(1,j))
	next j
OUT_THE_LOOP: !
	fnTos
	mylen=20 : mypos=mylen+2
	fnLbl(1,1,'Filter:',mylen,1)
	fnTxt(1,mypos,18)
	resp$(1)=filter$
	fnLbl(2,1,'Directory:',mylen,1)
	fnTxt(2,mypos,40,255)
	resp$(2)=dur$
	ch$(1)='File Name' : ch$(2)='Ext' : ch$(3)='Path'
	mat item$(3) : mat ch$(3) : mat cm$(3)
	fnflexinit1('loc-br',3,1,15,90,mat ch$,mat cm$,1)
	for j=1 to udim(brfn$)
		item$(1) = prog$(1,j)
		item$(2) = ext$(1,j)
		item$(3) = path$(1,j)
		fnflexadd1(mat item$)
	next j
	fnCmdSet(102)
L350: ckey=fnAcs(mat resp$)
	if ckey=5 then goto DONE
	filter$=resp$(1)
	dur$=resp$(2)
	if ckey=6 then goto REFREASH
 
	goto L350
 
DONE: !
 
	stop
 
include: ertn
 
