20140 ! Replace S:\Core\Ace\Win3B.br
50000   def library fnwin3b(win,&cap$,win_height,win_width;display_cnam,button_option,win_align,pr_newpg)
50020     library 'S:\Core\Library': fnerror,fnfkey
50040     on error goto ERTN
50060     dim temp_file$*80
50080 ! Win= Window number to be opened 
50100     ! if 0 win will default to 101
50120 ! Cap$= Caption to be used
50140 ! Win_Height and Win_Width= seems pretty obvious don't it
50160 ! Display_CNam= 1. Display Cnam and Cno in top 2 lines of window 
50180     !               2. Display only Cno in top 1 line
50200 ! button_option= 0. Does nothing. 
50220     !                1. Cancel(F5)  
50240     !                2. Next(F1),Cancel(F5) 
50260     !                3. Print(F1),Cancel(F5)  
50280     !                4. Save (F1),Cancel(F5) 
50300     !                5. Next (F1),Cancel(F5),Search(F6) 
50320     !                6. Next (F1),Back(F2),Cancel(F5) 
50340     !                7. Save (F1),Delete(F4),Cancel(F5) 
50360     !                8. pr (F1),Back(F2),Cancel(F5) 
50380     !                9. (unassigned)
50400 !               11. Next (F1), Finish (F5)
50420 !               41. OK 
50440     !               42. Yes (F1), No (F2) 
50460     !               43. Yes, No, Cancel 
50480     ! (NOTE: if button_option>0 and <40 is set to have Cancel(F5))
50500 !               51. Exit (F5) 
50520     !               52. Finish (F5)
50540 ! win_align= 1=bottom left; 2=bottom center; 3=bottom right 
50560     !            4=center left; 5=center center; 6=center right 
50580     !            7=top left;    8=top center;    9=top right    
50600     !                0=default saveable position (not finished yet)
50620 ! pr_newpg = 1=Yes; 2=No; 0=Whatever, I don't know
50640     if win<1 then win=101
50660
50680     if pr_newpg=2 then goto L240
50700     if pr_newpg=0 and exists(":C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L240 else pr newpage
50720     if pr_newpg=1 then pr newpage
50740 L240: screen_width=80
50760     screen_height=24
50780     win_width=min(screen_width-2,win_width)
50800     win_height=min(screen_height-2,win_height)
50840     on win_align gosub WIN_ALIGN_1,WIN_ALIGN_2,WIN_ALIGN_3,WIN_ALIGN_4,WIN_ALIGN_5,WIN_ALIGN_6,WIN_ALIGN_7,WIN_ALIGN_8,WIN_ALIGN_9 none WIN_ALIGN_0
50860     goto L490
50880 ! _____
50900 WIN_ALIGN_0: gosub WIN_ALIGN_5 : return 
50920 WIN_ALIGN_1: gosub H_LEFT : gosub V_BOTTOM : return 
50940 WIN_ALIGN_2: gosub H_CENTER : gosub V_BOTTOM : return 
50960 WIN_ALIGN_3: gosub H_RIGHT : gosub V_BOTTOM : return 
50980 WIN_ALIGN_4: gosub H_LEFT : gosub V_CENTER : return 
51000 WIN_ALIGN_5: gosub H_CENTER : gosub V_CENTER : return 
51020 WIN_ALIGN_6: gosub H_RIGHT : gosub V_CENTER : return 
51040 WIN_ALIGN_7: gosub H_LEFT : gosub V_TOP : return 
51060 WIN_ALIGN_8: gosub H_CENTER : gosub V_TOP : return 
51080 WIN_ALIGN_9: gosub H_RIGHT : gosub V_TOP : return 
51100 H_LEFT: sc=2 : ec=sc+win_width-1 : return 
51120 H_RIGHT: ec=screen_width-1 : sc=ec-win_width+1 : return 
51140 H_CENTER: !
51160     sc=int(((screen_width-win_width)/2)+1) 
51180     ec=sc+win_width-1 
51200     return 
51220 V_TOP: !
51240     sr=2 : er=sr+win_height-1 
51260     return 
51280 V_CENTER: sr=int(((screen_height-win_height)/2)+1) 
51300     er=sr+win_height-1 
51320     return 
51340 V_BOTTOM: !
51360     er=screen_height-1 : sr=er-win_height+1
51380     return  ! ____________
51400 L490: ! past win_align(s)
51420 ! 
51440     temp_file$=env$('temp')&"\Win-"&cnvrt$("pic(###)",win)&".tmp"
51460     close #win: ioerr ignore
51480     open #win: "Name="&temp_file$&",Size=0,RecL=80,Replace",internal,outIn 
51500     write #win,using 'Form POS 1,5*N 4': win,sc,ec,sr,er
51520     close #win: ioerr ignore
51540     open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
51560     pr #win: newpage
51580     if display_cnam=0 then goto L610
51600     if display_cnam=1 then 
51620       pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width)) 
51640       pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
51660     end if
51680     if display_cnam=2 then 
51700       pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
51720     end if
51740 L610: if button_option=0 then goto XIT
51760     mat fkey$=("") : em$="" : es=0
51780     fkey$(5)="Cancel" ! included by default
51800     if button_option=2 then fkey$(1)="Next"
51820     if button_option=3 then fkey$(1)="Print"
51840     if button_option=4 then fkey$(1)="Save"
51860     if button_option=5 then fkey$(1)="Next" : fkey$(6)="Search"
51880     if button_option=6 then fkey$(1)="Next" : fkey$(2)="Back"
51900     if button_option=7 then fkey$(1)="Save" : fkey$(4)="Delete"
51920     if button_option=8 then fkey$(1)="Print" : fkey$(2)="Back"
51940     if button_option=11 then fkey$(1)="Next" : fkey$(5)="Finish"
51960     if button_option>40 then fkey$(5)=""
51980     if button_option=41 then fkey$(1)="Ok"
52000     if button_option=42 then fkey$(1)="Yes" : fkey$(2)="No"
52020     if button_option=51 then fkey$(5)="Exit"
52040     if button_option=52 then fkey$(5)="Finish"
52060     fnfkey(er+1,mat fkey$,mat disfk,em$,es)
52080     goto XIT
52100 ! ______________________________________________________________________
52120 ! <Updateable Region: ERTN>
52140 ERTN: fnerror(program$,err,line,act$,"xit")
52160     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
52180     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
52200     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
52220 ERTN_EXEC_ACT: execute act$ : goto ERTN
52240 ! /region
52260 ! ______________________________________________________________________
52280 XIT: fnend 
60000 ! Replace S:\Core\fkey2.br
60020 ! puts buttons at the bottom of a console window
60040 ! ______________________________________________________________________
60060   def library fnfkey(scrline,mat fkey$,mat disfk,&em$,es)
60080 ! ______________________________________________________________________
60100     library 'S:\Core\Library': fnerror,fngethandle
60120     on error goto ERTN
60140 ! ______________________________________________________________________
60160     dim temp_file$*80
60180 ! ______________________________________________________________________
60200     totallen=startpos=0
60220     for j=1 to udim(fkey$) ! add ' (Fx)' to each button
60240       if fkey$(j)<>"" then 
60260         fkey$(j)=fkey$(j)&" (F"&str$(j)&")"  ! add ' (Fx)' to each button
60280         totallen+=len(fkey$(j))+1
60290       end if
60300     next j
60320     totallen+=len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
60340     totallen-=1
60360     if win=0 then goto NO_TEMP_FILE
60380     temp_file$=env$('temp')&'\'&"Win-"&cnvrt$("pic(###)",win)&".tmp"
60400     open #tfn:=fngethandle: "Name="&temp_file$&",RecL=80,use",internal,outIn ioerr NO_TEMP_FILE
60420     read #tfn,using 'Form POS 1,5*N 4': win_align,sc,ec,sr,er ioerr NO_TEMP_FILE
60440     startpos=int((ec-sc-totallen)/2+sc)+1
60460     goto L270
60480 NO_TEMP_FILE: !
60500     startpos=int((80-totallen)/2)+1
60520 L270: close #tfn: ioerr L280
60540 L280: pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
60560     for j=1 to udim(fkey$)
60580       if fkey$(j)="" then goto L350
60600       if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
60620       if disfk(j)=1 then goto L340
60640       pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
60660 L340: startpos+=len(fkey$(j))+1
60680 L350: next j
60700     if rtrm$(em$)<>"" then 
60720       pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
60740     end if
60760 fnend 
