00100 ! formerly S:\Core\Programs\SelCNo
00200 ! Select Company Number for the current system
00300 fn_setup
01600 fntop(program$)
14000 ! r: if cursys=CO than just pick the first thing they are licensed for
14020   if env$('cursys')='CO' and udim(mat client_has$)=>2 then
14040     cursys$=client_has$(2)
14060     fnreg_write(session$&'.CurSys',cursys$)
14080     fncursys$(cursys$)
14100     fn_setup_on_cursys_change
14120   end if
14140 ! /r
16000 fncno(cno)
20000 MENU1: ! r:
20020   fnTos(sn$:=env$('cursys')&"Companies")
21000   ! r: add the system buttons to the screen
21010   fnbutton_or_disabled(env$('enableClientSelection')=='Yes',2,2,env$('client')(1:37),fkey_client:=5201, 'Client Name is "'&env$('client')&'"',37)
21020   fnFra(4,1,udim(mat client_has$)+1,38, 'System','Click a system button to change and view the companies in that system')
21040   ch_line=1
21060   for ch_item=2 to udim(mat client_has$) ! starting at 2 to always skip CO = which is always #1
21080     if client_has$(ch_item)<>'U4' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U5' and client_has$(ch_item)<>'G2' then 
21100       ch_line+=1
21110       fnbutton_or_disabled((~env$('cursys')==client_has$(ch_item)),ch_line,1,fnSystemName$(client_has$(ch_item))(1:37),1000+ch_item, '',37,1)
21120       ! if env$('cursys')=client_has$(ch_item) then 
21140       !   fnLbl(ch_line,1,fnSystemName$(client_has$(ch_item))(1:37),37,2,0,1)
21160       ! else 
21180       !   fnButton(ch_line,1,fnSystemName$(client_has$(ch_item))(1:37),1000+ch_item,'',1,37,1)
21200       ! end if 
21220     end if 
26000   next ch_item
26020   ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
26040   ! /r
28030   !  r: add that company grid to the screen
28160   fnflexinit1(sn$&'_flex',3,42,10,60,mat colhdr$,mat colmask$,1)
34000   ! 
34020   ! temp$='[Q]\'&env$('cursys')&"mstr"
34040   fngetdir2('[Q]\'&env$('cursys')&"mstr",mat filename$,'/od /ta',"Company.*") ! fngetdir(temp$,mat filename$,empty$,"Company.*") ! /oe
34060   company_count=filename_item=0
34080   for filename_item=1 to udim(mat filename$)
34100     tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
34120     if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
34140       company_count+=1
34160       item$(1)=str$(tmp_cno)
34180       item$(2)=fn_cname_of_cno$(tmp_cno)
34200       fnflexadd1(mat item$)
34220       if tmp_cno=cno then 
34240         setenv('current_grid_row',str$(company_count))
34260       end if 
34280     end if 
34300   ACNO_CONV: ! 
34320   next filename_item
34340   ! /r
35000   fnLbl(1,42,"Company:",9,0)
36000   fnButton(2,52,"Con&figure",14,'Select highlighted company and go to configure it.  Returns here upon exit.',1,9) ! ,0,0,1)  ! fnCmdKey("&Select",1,1)
36010   fnButton(2,62,"&Select",10,'',1,9) ! ,0,0,1)  ! fnCmdKey("&Select",1,1)
36020   fnButton(2,72,"&Add",2,'',1,9) ! fnCmdKey("&Add",2)
36040   fnButton(2,82,"Co&py",3,'',1,9)
36060   if company_count=>2 then ! Delete only allowed if there are 2 or more companies on the list
36080     fnButton(2,92,"&Delete",4,'',1,9)
36100   end if 
36110   fnButton(1,102,"Open...",17,'',1,9)
36120   if (fnclient_is_converting or company_count=0) then 
36140     fnButton(2,102,"I&mport",13,'',1,9)
36160   end if 
37000   fnCmdKey("&Save",15,1,0)
37020   if exists('[Q]\'&env$('cursys')&"mstr\Company.h"&str$(cno)) then ! cancel only allowed if they have not deleted their current company
37040     fnCmdKey("&Cancel",5,0,1)
37060   end if 
37080   fnAcs(sn$,win,mat resp$,ck)
38000   ! 
38020   if ck=5 and exists('[Q]\'&env$('cursys')&"mstr\Company.h"&str$(cno)) then ! cancel
38040     goto XIT
38060   else if ck=2 then 
38080     goto COMPANY_ADD
38082   else if ck=17 then ! Open...
38084     fnOpenPartial
38086     chain program$
38100   else if ck=13 and (fnclient_is_converting or company_count=0) then ! import
38120     fnchain('S:\Core\Company Import.br')
38140   end if 
39000   ! 
39020   cno_selected=val(resp$(1))
39040   ! 
40000   if ck=3 then ! Copy
40020     fn_company_copy(cno_selected)
40040   !   goto MENU1
40042   else if ck=14 then ! Delete Company
40043     gosub SELECT_COMPANY
40044     fn_company_configure(scno)
40060   else if ck=4 and company_count=>2 then ! Delete Company
40080     fn_company_delete(cno_selected)
40100   !   goto MENU1
40120   else if ck=10 then ! Select that Company
40140     gosub SELECT_COMPANY
40160     chain program$
41000   else if ck>1000 and ck<1200 then ! Select System 1001-1199 reserved for system selection
41020     cursys$=client_has$(ck-1000)
41040     fnreg_write(session$&'.CurSys',cursys$)
41060     fncursys$(cursys$)
41080     fn_setup_on_cursys_change
41100     chain program$
41120   else if fkey_client>0 and ck=fkey_client then
41140     fnClientSelect
41160     fn_system_setup
41180     if udim(mat client_has$)=>2 and srch(mat client_has$,env$('cursys'))<=0 then ! change it to the second available system (1st is CO) if they don't have the currently selected system
41200       fncursys$(client_has$(2))
41220       fn_setup_on_cursys_change
41240     end if
41260   else if ck=15 then ! SAVE
41280     gosub SELECT_COMPANY
41300     goto XIT
41320   end if 
41340   goto MENU1 ! /r
42000 SELECT_COMPANY: ! r:
42020   fnputcno(cno_selected)
42080 return  ! /r
43000 ! ______________________________________________________________________
44000 COMPANY_ADD: ! r:
44020   fnTos(sn$='selcno-'&env$('cursys')&'-2')
44040   mylen=25 : mypos=mylen+2
44060   respc=0
44080   fnLbl(1,1,"Company Number to add:",mylen,1)
44100   fnTxt(1,mypos,5,0,0,"30")
44120   resp$(respc+=1)="0"
44140   fnCmdSet(2)
44160   fnAcs(sn$,0,mat resp$,ck)
44180   if ck=5 then goto MENU1
44200   cno_selected=val(resp$(1))
44220   if fn_company_already_exists(cno_selected)=1 then goto MENU1
45000   fnputcno(cno_selected)
45020   fncheckfileversion
45040   if env$('cursys')='PR' or env$('cursys')='SU' or env$('cursys')='TM' or env$('cursys')='CL' then ! no AddCNo necessary - just copy in from *.h99999 and go straight to Company Information
45060     if exists('S:\'&fnSystemName$&'\mstr\*.h99999') then
45080       fnCopy('S:\'&fnSystemName$&'\mstr\*.h99999','[Q]\'&env$('cursys')&'mstr\*.h[cno]')
45100     else if exists('S:\acs'&env$('cursys')&'\mstr\*.h99999') then
45120       fnCopy('S:\acs'&env$('cursys')&'\mstr\*.h99999','[Q]\'&env$('cursys')&'mstr\*.h[cno]')
45140     end if
45160     if env$('cursys')='CL' then ! r: special processing for CL
45180       mat ml$(5)
45200       ml$(1)='Would you like to import data from an old'
45220       ml$(2)='ACS Accounts Payable system?'
45240       ml$(3)='This is only chance.'
45260       fnmsgbox(mat ml$,resp$,'',36)
45280       if resp$='Yes' then 
45300         fnApMstrConversion
45320         fnchain("S:\acsCL\Conversion\GLBLD-CNV")
45340       end if
45360     end if  ! /r
45380     fnchain("S:\acs"&env$('cursys')&"\Company")
45400   else if env$('cursys')='UB' then
45420     fnchain(txt$="S:\Core\AddCNo")
45440   else if exists('S:\'&fnSystemName$(env$('cursys'))&'\Company.br') then
45460     fnchain('S:\'&fnSystemName$(env$('cursys'))&'\Company.br') 
45480   else 
45500     fnchain(txt$="S:\acs"&env$('cursys')&"\AddCNo")
45520   end if 
45540 ! /r
46000 XIT: fnxit
47000 IGNORE: continue 
48000 ! <Updateable Region: ERTN>
48020 ERTN: fnerror(program$,err,line,act$,"xit")
48040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
48060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
48080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
48100 ERTN_EXEC_ACT: execute act$ : goto ERTN
48120 ! /region
50000 def fn_company_already_exists(cae_cno)
50020   cae_return=0
50040   if exists('[Q]\'&env$('cursys')&"mstr\Company.h"&str$(cae_cno)) then 
50060     cae_return=1
50080     ! mat mg$(3)
50100     mat mg$(2)
50120     mg$(1)="Company number "&str$(cae_cno)&". "&rtrm$(fn_cname_of_cno$(cae_cno))&" already exist!"
50140     mg$(2)="Would you like to erase it first?"
50160      fnmsgbox(mat mg$,response$,'',32+4+256) ! (i)+(yn)+(secondButtonDefault)
50180     if response$='Yes' then
50200       if fn_company_delete(cae_cno) then cae_return=0
50220     end if
50240     !  mg$(2)="Select a different company number"
50260     !  mg$(3)="or exit and erase the existing company first."
50280     !  fnmsgbox(mat mg$,response$,'',16)
50300   end if 
50320   fn_company_already_exists=cae_return
50340 fnend 
52000 def fn_cname_of_cno$*40(cno)
52020   dim coc_return$*40
52040   coc_return$=''
52060   open #h_tmp:=fngethandle: "Name=[Q]\"&env$('cursys')&"mstr\Company.h"&str$(cno),internal,input ioerr COC_FINIS
52080   read #h_tmp,using "Form pos 1,c 40": coc_return$
52100   close #h_tmp: 
52120   COC_FINIS: ! 
52140   fn_cname_of_cno$=coc_return$
52160 fnend 
62000 def fn_company_delete(cno)
62010   companyDeleteReturn=0
62020   fnTos(sn$="company_delete")
62040   fnLbl(1,1,"**** WARNING ****",40,1,5)
62060   fnLbl(3,1,"You have chosen to completely delete company:",60,2)
62080   fnLbl(4,1,str$(cno)&". "&fn_cname_of_cno$(cno),60,2)
62100   fnLbl(6,1,"The only way to get it back will be to restore a backup.",60,2)
62140   fnLbl(9,11,"Enter ERASE to continue:",24,1)
62160   fnTxt(9,36,5)
62180   resp$(1)=""
62200   fnCmdSet(2)
62220   fnAcs(sn$,0,mat resp$,ckey)
62240   e$=uprc$(trim$(resp$(1)))
62260   if ckey<>5 then 
62280     if e$="ERASE" then 
62300       fnFree('[Q]\'&env$('cursys')&"mstr"&"\*.h"&str$(cno))
62320       mat mg$(1)
62340       mg$(1)='Company Number '&str$(cno)&' has been Deleted!'
62360       fnmsgbox(mat mg$,resp$,'',0)
62370       companyDeleteReturn=1
62380     end if 
62400   end if 
62410   fn_company_delete=companyDeleteReturn
62420 fnend 
66000 def fn_company_configure(scno)
66020   setenv("xit_override","")
66040   setenv("xit_override","S:\Core\Programs\Select Company")
66060   fnchain('S:\acs'&env$('cursys')&'\Company.br')
66080 fnend 
72000 def fn_company_copy(scno)
72020   dim dcnam$*40
72040   dcno=0
72050   dcnam$=fn_cname_of_cno$(scno)
72060   CC_SCREEN1: ! 
72080   fnTos(sn$='CopyCNo3')
72100   lc=0
72120   mylen=29 : mypos=mylen+2
72140   fnLbl(lc+=1,1,"Source Company:",mylen,1)
72160   fnLbl(lc,mypos,str$(scno)&'. '&fn_cname_of_cno$(scno),50)
72180   lc+=1
72200   fnLbl(lc+=1,1,"&Destination Company Number:",mylen,1)
72220   fnTxt(lc,mypos,5,0,0,'30')
72240   resp$(1)=str$(dcno)
72260   fnLbl(lc+=1,1,"Destination Company Name:",mylen,1)
72280   fnTxt(lc,mypos,40,40)
72300   resp$(2)=dcnam$
72320   lc+=1
72340   fnLbl(lc+=1,1,"Warning",80,2,1)
72360   fnLbl(lc+=1,1,"Please make sure no one else is",80,2)
72380   fnLbl(lc+=1,1,"using either company number.",80,2)
72400   fnLbl(lc+=1,1,"If the destination company exists",80,2)
72420   fnLbl(lc+=1,1,"it will be overwritten!",80,2)
72440   fnCmdSet(2)
72460   fnAcs(sn$,0,mat resp$,ck)
72480   if ck<>5 then 
72500     dcno=val(resp$(1))
72520     dcnam$=resp$(2)
72540     if fn_company_already_exists(dcno)=1 then goto CC_SCREEN1
72560     fnCopy("[Q]\"&env$('cursys')&"mstr"&"\*.h"&str$(scno),"[Q]\"&env$('cursys')&"mstr"&"\*.h"&str$(dcno))
72580     ! fnCopy("S:\acs"&env$('cursys')&"\*.h"&str$(scno),"S:\acs"&env$('cursys')&"\*.h"&str$(dcno))
72600     if uprc$(env$('cursys'))=uprc$("UB") then 
72620       fnCopy("[Q]\UBmstr\ubData\*.h"&str$(scno),"[Q]\UBmstr\ubData\*.h"&str$(dcno))
72640     end if 
72660     fn_update_company_name(dcno,dcnam$)
72680   end if 
72700 fnend 
74000 def fn_update_company_name(cno,cnam$*40)
74020   cnam$=rtrm$(cnam$)
74040   if cnam$<>'' then 
74060     open #h_company:=fngethandle: 'Name=[Q]\'&env$('cursys')&'mstr\Company.h'&str$(cno),internal,outIn,relative 
74080     rewrite #h_company,using 'form pos 1,c 40',rec=1: cnam$
74100     close #h_company: 
74120   end if 
74140 fnend 
76000 def fn_setup
76020   if setup<>1 then 
76040     setup=1
76060     library 'S:\Core\Library': fnAcs,fnerror,fncno,fnTos,fnchain,fnxit,fnputcno,fngetdir2
76080     library 'S:\Core\Library': fncursys$,fnLbl,fnCmdSet,fntop,fnTxt,fnCmdKey,fncheckfileversion
76100     library 'S:\Core\Library': fnmsgbox,fnflexadd1,fnflexinit1,fngethandle,fnclient_is_converting
76120     library 'S:\Core\Library': fnclient_has_mat,fnreg_read,fnreg_write
76140     library 'S:\Core\Library': fnButton,fnFra,fnSystemName$
76160     library 'S:\Core\Library': fnApMstrConversion
76180     library 'S:\Core\Library': fnbutton_or_disabled
76190     library 'S:\Core\Library': fnClientSelect
76192     library 'S:\Core\Library': fnOpenPartial
76194     library 'S:\Core\Library': fnCopy,fnFree
76200     on error goto ERTN
76220     ! ______________________________________________________________________
76240     dim filename$(999)*40
76260     dim resp$(10)*50,txt$*40 ! ,temp$*256
76280     dim mg$(3)*128
76300     ! r: NOTE: Regardless of which ACS System:
76320     !  * Add Company program must be called acs[cursys]\ADDCNO.br
76340     !  * Company files must be named Company.hxx
76360     ! /r
76380   end if 
76400   ! 
76420   ! r: constants and variables setup for the company grid
76440     mat colhdr$(2)
76460     colhdr$(1)='Number'
76480     colhdr$(2)='Name'
76500     mat colmask$(2)
76520     colmask$(1)="30"
76540     colmask$(2)=""
76560     dim item$(2)*40
76580   ! /r
76600   fn_system_setup
76620 fnend 
78000 def fn_system_setup
78020   if ~fnclient_has_mat(mat client_has$) and env$("ACSDeveloper")="" then 
78030     dim ml$(1)*128
78140     mat ml$(4)
78160     ml$(1)='Client '&env$('client')&' has nothing licensed.  Please perform an update.'
78180     ml$(2)='If you have already performed an update and are'
78200     ml$(3)='still receiving this message contact ACS at 1-800-643-6318'
78220     ml$(4)='Perform an Update now?'
78240     fnmsgbox(mat ml$,resp$,'',16+4)
78260     if uprc$(resp$)=uprc$("Yes") then 
78280       chain 'S:\Core\Programs\Update'
78300     else 
78320       goto XIT
78340     end if 
78360   end if 
78380   ! 
78400   dim cursys$*40
78420   !  cursys$=fncursys$
78440   fnreg_read(session$&'.CurSys',cursys$)
78480   cursys$=fncursys$(cursys$)
78500   fn_setup_on_cursys_change
78520 fnend 
80000 def fn_setup_on_cursys_change
80020   dim cnam$*80
80040   fncno(cno,cnam$)
80060   if cno=0 then 
80080     cno=1
80100     fnputcno(cno)
80120     fncno(cno,cnam$)
80140   end if 
80160   ! 
80180   if ~exists('[Q]\'&cursys$&'mstr') and cursys$<>'CO' then execute 'mkdir "[Q]\'&cursys$&'mstr"'
80200   ! if ~exists('[Q]\INI\acs'&cursys$) then execute 'mkdir [Q]\INI\acs'&cursys$
80220 fnend 
