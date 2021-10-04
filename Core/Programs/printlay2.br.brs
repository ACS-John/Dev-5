! Replace S:\Core\Programs\PrintLay2
! pr Layout(s)
 
	autoLibrary
	on error goto Ertn
 
	dim a$(200,3)*80,h1$*55,rm$(4)*128,filename$*50,ln$*120
	dim a(200,6),a$*132,prg$*20,mo$(12),cap$*128,source_path$*150
 
	fnTop("S:\Core\PrintLay",cap$="Print Layout(s)")
	data January,February,March,April,May,June,July,August,September,October,November,December
	read mat mo$
 
	fnconsole(on=1)
	dat$=mo$(val(date$("MM")))&" "&date$("DD")&", "&date$("CCYY")
	fnwin3b(win=101,cap$,4,65,0,2)
	pr #win,fields "2,2,Cr 18,N": "Path and Filename:"
	if wbversion$<'4.20hi' then io1$(1)="2,21,C 40/150,U" else io1$(1)="2,21,40/C 150,U"
 
! source_path$="S:\acsPR\Layouts\rptrail.lay"
	source_path$="S:\Client Billing\Legacy\Layouts\*.lay"
! source_path$="S:\acsUB\Layouts\*.lay"
! source_path$="S:\acsUB\Layouts\UBmstr-vb.lay"
L210: !
	rinput #win,fields io1$(1): source_path$
	if cmdkey=5 then goto Xit
	source_path$=rtrm$(source_path$)
	if source_path$="" then goto L210
	fn_printlay3(source_path$) : goto L210
def fn_old
		fnopenprn
		execute "DROP DirFile" ioerr ignore
		execute "DIR "&source_path$&" >DirFile"
		open #2: "Name=DirFile",display,input
		p1=pos(source_path$,"/",1)
L330: f2=f2+1: rl=l=a=col_6=ino=pg=j3=0: mat a=(0)
		linput #2: a$ eof L1380
		f2=1: f1=2
		open #1: "Name="&source_path$,display,input ioerr L330
		goto OLD_READ_LINE
OLD_READ_LINE: !
		linput #1: ln$ eof L1180
		if pos(srep$(ln$,'^','~'),'~')<=0 and pos(uprc$(ln$),'DATA ')>0 then ln$=srep$(ln$,',','^')
		if uprc$(ln$(7:10))=uprc$("LET ") then goto LETLN
		if uprc$(ln$(7:10))=uprc$("DATA") then goto DATALN
		if uprc$(ln$(7:7))=uprc$("!") then pr #255: ln$(9:len(ln$))
		p1=pos(ln$,"REPLACE",6)
		if p1=0 then goto L690
		p1=p1+8
		p2=pos(ln$,",",p1)
		prg$=ln$(p1:p2-1)
L690: goto OLD_READ_LINE
 
LETLN: p2=len(rtrm$(ln$))-1
		p1=pos(ln$,"H1$",1)
		if p1>0 then h1$=ln$(p1+5:p2) : goto OLD_READ_LINE
		p1=pos(ln$,"FILETYPE$",1)
		if p1>0 then filetype$=ln$(p1+11:p2): goto OLD_READ_LINE
		p1=pos(ln$,"FILENAME$",1)
		if p1>0 then filename$=ln$(p1+11:p2): goto OLD_READ_LINE
		p1=pos(ln$,"VOLID$",1)
		if p1>0 then volid$=ln$(p1+8:p2): goto OLD_READ_LINE
		p1=pos(ln$,"RM$",1)
		if p1=0 then goto L840
		rm=val(ln$(p1+4:p1+4))
		rm$(rm)=ln$(p1+8:p2)
L840: goto OLD_READ_LINE
 
DATALN: !
		j3=j3+1
		p1=11
		p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,"^",p1+1)
		p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,"^",p2+1)
		p4=len(rtrm$(ln$))
		a$(j3,1)=ln$(p1:p2-1)
		a$(j3,2)=ln$(p2+1:p3-1)
		a$(j3,3)=ln$(p3+1:p4)
L940: form c 9,skip 0
		if rtrm$(a$(j3,3))="" then goto L1160
		p1=pos(a$(j3,3)," ",1)+1
		p2=pos(a$(j3,3),".",1)+1
		p3=len(rtrm$(a$(j3,3)))
		p4=pos(a$(j3,3),"*",1)
		if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
		l=int(val(a$(j3,3)(p1:pos(srep$(a$(j3,3),'^','~'),'~')-1))) ! FIELD STORAGE LENGTH
! pos(srep$(a$(j3,3),'^','~'),'~')-1         was      P3
		if p2>1 then dp=val(a$(j3,3)(p2:pos(srep$(a$(j3,3),'^','~'),'~')-1)) else dp=0 ! DECIMAL POS.
! pos(srep$(a$(j3,3),'^','~'),'~')-1      was     P3
		if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l !   ACTUAL FIELD LENGTH
		l=l*m1 ! TOTAL STORAGE LENGTH
		col_6=a+l
		a=a+1
		ino=ino+1
		a(j3,1)=ino
		a(j3,2)=al
		a(j3,3)=dp
		a(j3,4)=l
		a(j3,5)=a
		a(j3,6)=col_6
		a=col_6
		rl=rl+int(val(a$(j3,3)(p1:pos(srep$(a$(j3,3),'^','~'),'~')-1)))*m1 ! pos(srep$(a$(j3,3),'^','~'),'~')-1      was    P3
L1160: goto OLD_READ_LINE
 
L1180: pgo=ceil(j3/24)
		gosub HDR
		for j=1 to j3
			p1=pos(a$(j,3)," ",1)
			p2=len(a$(j,3))
			l=val(a$(j,3)(p1:pos(srep$(a$(j,3),'^','~'),'~')-1)) ! pos(srep$(a$(j,3),'^','~'),'~')-1   was    P2
			if l>0 then goto L1290
			pr #255,using L1270: a$(j,1)
L1270: form pos 13,c 43,skip 2
			goto L1330
L1290: if rtrm$(a$(j,1))="" then goto L1330
			a$(j,3)=a$(j,3)(1:pos(srep$(a$(j,3),'^','~'),'~')-1)
			pr #255,using L1320: a(j,1),a$(j,1),a$(j,2),a(j,2),a(j,3),a$(j,3),a(j,4),a(j,5),a(j,6)
L1320: form pos 5,n 5,x 3,c 43,c 21,n 7,n 10,x 5,c 11,n 7,2*n 9,skip 1 ! c 11 was c 11
L1330: next j
		pr #255,using L940: hex$("1B40")
		close #1:
		if f2<f1 then goto L330
L1380: close #2,free:
		fncloseprn
		goto Xit
 
Xit: fnXit
 
HDR: !
	pg+=1
	pr #255,using L1520: h1$
	L1520: form pos 3,c 46
	pr #255,using L1540: prg$,dat$,pg,pgo
	L1540: form pos 51,c 40,c 20,"Page ",n 3,"  of ",n 3
	pr #255,using L1560: str$(rl),rm$(1)
	L1560: form pos 5,"Record Length: ",c 5,pos 30,c 45
	pr #255,using L1590: rm$(2)
	pr #255,using L1590: rm$(3)
	L1590: form pos 30,c 45
	pr #255,using L1610: filetype$,rm$(4)
	L1610: form pos 5,"File Type: ",c 10,pos 30,c 128
	pr #255,using L1630: filename$,volid$
	L1630: form pos 5,"File Name: ",c 50,"Directory: ",c 10
	pr #255: ""
	pr #255: "   Item #   Field Description                           Name                 Length    Decimals    Format    Storage     From      To  "
	pr #255: "   ______   ________________________________________   __________________    ______    ________   _______    _______    _____    _____"
return
fnend
 
include: ertn
 
def fn_file_to_array(fta_file$*256,mat fta_line$)
		open #1: 'Name='&fta_file$,display,input
		fta_line_item=0
		do
			fta_line_item+=1
			mat fta_line$(fta_line_item)
			linput #1: fta_line$(fta_line_item) eof FTA_DONE
		loop
FTA_DONE: !
		close #1:
fnend  ! fn_file_to_array
def fn_printlay3(source_path$*256)
		dim p3_filename$(1)*256,p3_filter$*20
		if pos(source_path$,'*')>0 or pos(source_path$,'?')>0 then
			source_path$=trim$(source_path$)
			p3_pos_slash_last=pos(source_path$,'\',-1)
			p3_pos_slash_last=pos(source_path$,'\',-1)
			p3_filter$=source_path$(p3_pos_slash_last+1:len(source_path$))
			source_path$=source_path$(1:p3_pos_slash_last)
			fngetdir2(source_path$,mat p3_filename$,'',p3_filter$)
		else
			p3_filename$(1)=rtrm$(source_path$,'\')
			source_path$=''
		end if  !
		dim p3_line$(1)*256
		fnopenprn
P3_FORM_PRINT: form pos 1,g 4,x 1,c 5,x 1,c 5,x 1,c 30,x 1,c 80
		for p3_file_item=1 to udim(mat p3_filename$)
!  if trim$(p3_filename$(p3_file_item))=trim$(source_path$) then source_path$=''
			fn_file_to_array(source_path$&p3_filename$(p3_file_item),mat p3_line$)
			if p3_file_item>1 then pr #255: newpage
			pr #255: os_filename$(source_path$&p3_filename$(p3_file_item))
			p3_spos=1
			for p3_line_item=1 to udim(mat p3_line$)
				dim p3_tmp$*256
				p3_tmp$=p3_line$(p3_line_item)
				p3_tmp$(1:6)='' ! strip line number and leading space
				if lwrc$(p3_tmp$(1:5))='data ' then
					if p3_spos=1 then
					  pr #255,using P3_FORM_PRINT: 'spos','fmt','len','var','description'
					end if  ! p3_spos=1
					p3_tmp$(1:5)=''
					p3_tmp$=srep$(p3_tmp$,chr$(9),' ')
! ie remaing data might be something like:  Customer Number^Z$^C 10^Customer #
					dim p3_tmp_field$(1)*80
					str2mat(p3_tmp$,mat p3_tmp_field$,'^')
					if udim(p3_tmp_field$)<3 then
					  pr #255: '    '&p3_tmp$
					else
					  str2mat (p3_tmp_field$(3),mat p3_tmp_format$,' ')
					  str2mat (p3_tmp_field$(3),mat p3_tmp_format$,' ')
					  pr #255,using P3_FORM_PRINT: p3_spos,p3_tmp_format$(1),p3_tmp_format$(2),p3_tmp_field$(2),p3_tmp_field$(1)
					  p3_pos_star=pos(p3_tmp_format$(1),'*')
					  if p3_pos_star<=0 then
					    p3_spos+=int(val(p3_tmp_format$(2)))
					  else
					    p3_tmp=int(val(p3_tmp_format$(2)))
					    p3_tmp=p3_tmp*val(p3_tmp_format$(1)(1:p3_pos_star-1))
					    p3_spos+=p3_tmp
					  end if  ! p3_pos_star<=0   /   else
					end if  ! udim(p3_tmp_field$)=1   /   else
				else
					pr #255: p3_tmp$
				end if  ! lwrc$(p3_tmp$(1:5))='data '   /   else
			next p3_line_item
		next p3_file_item
		fncloseprn
fnend  ! fn_printlay3
