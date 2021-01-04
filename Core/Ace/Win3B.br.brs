! r: testing
autoLibrary
fntop(program$)
dim tmp$*128
fnWin3b(101,tmp$=env$('program_captopn'),10,70, 1,43)
pause
end
! /r
def library fnWin3b(win,&cap$,win_height,win_width; display_cnam,button_option,win_align,pr_newpg)
	autoLibrary
	on error goto Ertn
	fnWin3b=fn_win3b(win,cap$,win_height,win_width, display_cnam,button_option,win_align,pr_newpg)
fnend
def fn_win3b(win,&cap$,win_height,win_width; display_cnam,button_option,win_align,pr_newpg)
	dim temp_file$*80
	! Win= Window number to be opened
	! if 0 win will default to 101
	! Cap$= Caption to be used
	! Win_Height and Win_Width= seems pretty obvious don't it
	! Display_CNam= 1. Display Cnam and Cno in top 2 lines of window
	!               2. Display only Cno in top 1 line
	! button_option= 0. Does nothing.
	!                1. Cancel(F5)
	!                2. Next(F1),Cancel(F5)
	!                3. Print(F1),Cancel(F5)
	!                4. Save (F1),Cancel(F5)
	!                5. Next (F1),Cancel(F5),Search(F6)
	!                6. Next (F1),Back(F2),Cancel(F5)
	!                7. Save (F1),Delete(F4),Cancel(F5)
	!                8. pr (F1),Back(F2),Cancel(F5)
	!                9. (unassigned)
	!               11. Next (F1), Finish (F5)
	!               41. OK
	!               42. Yes (F1), No (F2)
	!               43. Yes, No, Cancel
	! (NOTE: if button_option>0 and <40 is set to have Cancel(F5))
	!               51. Exit (F5)
	!               52. Finish (F5)
	! win_align= 1=bottom left; 2=bottom center; 3=bottom right
	!            4=center left; 5=center center; 6=center right
	!            7=top left;    8=top center;    9=top right
	!                0=default saveable position (not finished yet)
	! pr_newpg = 1=Yes; 2=No; 0=Whatever, I don't know
	if win<1 then win=101

	if pr_newpg=2 then
		goto L240
	else if pr_newpg=0 and exists(":C:\ACS\Local\Settings\No_Print_Newpage.txt") then
		goto L240
	else
		pr newpage
	end if
	if pr_newpg=1 then pr newpage
	L240: !
	screen_width=80
	screen_height=24
	win_width=min(screen_width-2,win_width)
	win_height=min(screen_height-2,win_height)
	on win_align gosub WIN_ALIGN_1,WIN_ALIGN_2,WIN_ALIGN_3,WIN_ALIGN_4,WIN_ALIGN_5,WIN_ALIGN_6,WIN_ALIGN_7,WIN_ALIGN_8,WIN_ALIGN_9 none WIN_ALIGN_0
	goto L490

	WIN_ALIGN_0: gosub WIN_ALIGN_5 : return
	WIN_ALIGN_1: gosub H_LEFT : gosub V_BOTTOM : return
	WIN_ALIGN_2: gosub H_CENTER : gosub V_BOTTOM : return
	WIN_ALIGN_3: gosub H_RIGHT : gosub V_BOTTOM : return
	WIN_ALIGN_4: gosub H_LEFT : gosub V_CENTER : return
	WIN_ALIGN_5: gosub H_CENTER : gosub V_CENTER : return
	WIN_ALIGN_6: gosub H_RIGHT : gosub V_CENTER : return
	WIN_ALIGN_7: gosub H_LEFT : gosub V_TOP : return
	WIN_ALIGN_8: gosub H_CENTER : gosub V_TOP : return
	WIN_ALIGN_9: gosub H_RIGHT : gosub V_TOP : return
	H_LEFT: sc=2 : ec=sc+win_width-1 : return
	H_RIGHT: ec=screen_width-1 : sc=ec-win_width+1 : return
	H_CENTER: !
		sc=int(((screen_width-win_width)/2)+1)
		ec=sc+win_width-1
	return
	V_TOP: !
		sr=2 : er=sr+win_height-1
	return
	V_CENTER: sr=int(((screen_height-win_height)/2)+1)
		er=sr+win_height-1
	return
	V_BOTTOM: !
		er=screen_height-1 : sr=er-win_height+1
	return
	L490: ! past win_align(s)

	temp_file$=env$('temp')&"\Win-"&cnvrt$("pic(###)",win)&".tmp"
	close #win: ioerr ignore
	open #win: "Name="&temp_file$&",Size=0,RecL=80,Replace",internal,outIn
	write #win,using 'Form POS 1,5*N 4': win,sc,ec,sr,er
	close #win: ioerr ignore
	open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn
	pr #win: newpage
	if display_cnam=0 then goto L610
	if display_cnam=1 then
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
		pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
	end if
	if display_cnam=2 then
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
	end if
	L610: !
	if button_option=0 then goto Xit
	mat fkey$=("") : em$="" : es=0
	fkey$(5)="Cancel" ! included by default
	if button_option=2 then fkey$(1)="Next"
	if button_option=3 then fkey$(1)="Print"
	if button_option=4 then fkey$(1)="Save"
	if button_option=5 then fkey$(1)="Next" : fkey$(6)="Search"
	if button_option=6 then fkey$(1)="Next" : fkey$(2)="Back"
	if button_option=7 then fkey$(1)="Save" : fkey$(4)="Delete"
	if button_option=8 then fkey$(1)="Print" : fkey$(2)="Back"
	if button_option=11 then fkey$(1)="Next" : fkey$(5)="Finish"
	if button_option>40 then fkey$(5)=""
	if button_option=41 then fkey$(1)="Ok"
	if button_option=42 then fkey$(1)="Yes" : fkey$(2)="No"
	if button_option=51 then fkey$(5)="Exit"
	if button_option=52 then fkey$(5)="Finish"
	fn_fkey(er+1,mat fkey$,mat disfk,em$,es)
goto Xit
Xit: fnend
def library fnFkey(scrline,mat fkey$,mat disfk,&em$,es)
	autoLibrary
	on error goto Ertn
	fnFkey=fn_fkey(scrline,mat fkey$,mat disfk,em$,es)
fnend
def fn_fkey(scrline,mat fkey$,mat disfk,&em$,es; _
	___,temp_file$*80,totallen,startpos)
	! puts buttons at the bottom of a console window
	for j=1 to udim(mat fkey$) ! add ' (Fx)' to each button
		if fkey$(j)<>"" then
			fkey$(j)=fkey$(j)&" (F"&str$(j)&")"  ! add ' (Fx)' to each button
			totallen+=len(fkey$(j))+1
		end if
	next j
	totallen+=len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
	totallen-=1
	if win=0 then goto NO_TEMP_FILE
	temp_file$=env$('temp')&'\'&"Win-"&cnvrt$("pic(###)",win)&".tmp"
	open #tfn=fnH: "Name="&temp_file$&",RecL=80,use",internal,outIn ioerr NO_TEMP_FILE
	read #tfn,using 'Form POS 1,5*N 4': win_align,sc,ec,sr,er ioerr NO_TEMP_FILE
	startpos=int((ec-sc-totallen)/2+sc)+1
	goto L270
	NO_TEMP_FILE: !
	startpos=int((80-totallen)/2)+1
	L270: !
	close #tfn: ioerr ignore
	pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
	for j=1 to udim(mat fkey$)
		if fkey$(j)="" then goto L350
		if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
		if disfk(j)=1 then goto L340
		pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
		L340: startpos+=len(fkey$(j))+1
		L350: !
	next j
	if rtrm$(em$)<>"" then
		pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
	end if
fnend

include: ertn