00100 ! Replace R:\Core\Programs\SelCNo
00200 ! Select Company Number for the current system
00300   let fn_setup
01600   let fntop('R:\Core\Programs\SelCNo',cap$="Select Company Number ("&fncursys$&")")
01700   let fncno(cno)
20000 MENU1: ! r:
20020   let fntos(sn$:=fncursys$&"Companies")
21000 ! r: add the system buttons to the screen
21020   let fnfra(3,1,udim(mat client_has$)+1,38, 'System','Click a system button to change and view the companies in that system')
21040   let ch_line=1
21060   for ch_item=2 to udim(mat client_has$) ! starting at 2 to always skip CO = which is always #1
21080     if client_has$(ch_item)<>'U4' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U5' and client_has$(ch_item)<>'G2' then 
21100       let ch_line+=1
21120       if fncursys$=client_has$(ch_item) then 
21140         let fnlbl(ch_line,1,fnsystem_abbr_2_name$(client_has$(ch_item))(1:37),37,2,0,1)
21160       else 
21180         let fnbutton(ch_line,1,fnsystem_abbr_2_name$(client_has$(ch_item))(1:37),1000+ch_item,'',1,37,1)
21200       end if 
21220     end if 
26000   next ch_item
26020 ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
26040 ! /r
28030 !  r: add that company grid to the screen
28160   let fnflexinit1(sn$&'_flex',3,42,10,60,mat colhdr$,mat colmask$,1)
34000 ! 
34020 ! let temp$='Q:\'&fncursys$&"mstr"
34040   let fngetdir2('Q:\'&fncursys$&"mstr",mat filename$,'/od /ta',"Company.*") ! fngetdir(temp$,mat filename$,empty$,"Company.*") ! /oe
34060   let company_count=filename_item=0
34080   for filename_item=1 to udim(mat filename$)
34100     let tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
34120     if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
34140       let company_count+=1
34160       let item$(1)=str$(tmp_cno)
34180       let item$(2)=fn_cname_of_cno$(tmp_cno)
34200       let fnflexadd1(mat item$)
34220       if tmp_cno=cno then 
34240         let setenv('current_grid_row',str$(company_count))
34260       end if 
34280     end if 
34300 ACNO_CONV: ! 
34320   next filename_item
34340 ! /r
35000   let fnlbl(1,42,"Company:",9,0)
36000   let fnbutton(2,52,"Con&figure",14,'Select highlighted company and go to configure it.  Returns here upon exit.',1,9) ! ,0,0,1)  ! let fncmdkey("&Select",1,1)
36010   let fnbutton(2,62,"&Select",10,'',1,9) ! ,0,0,1)  ! let fncmdkey("&Select",1,1)
36020   let fnbutton(2,72,"&Add",2,'',1,9) ! let fncmdkey("&Add",2)
36040   let fnbutton(2,82,"Co&py",3,'',1,9)
36060   if company_count=>2 then ! Delete only allowed if there are 2 or more companies on the list
36080     let fnbutton(2,92,"&Delete",4,'',1,9)
36100   end if 
36120   if (fnclient_is_converting or company_count=0) then 
36140     let fnbutton(2,102,"I&mport",13,'',1,9)
36160   end if 
37000   let fncmdkey("&Save",15,1,0)
37020   if exists('Q:\'&fncursys$&"mstr\Company.h"&str$(cno)) then ! cancel only allowed if they have not deleted their current company
37040     let fncmdkey("&Cancel",5,0,1)
37060   end if 
37080   let fnacs(sn$,win,mat resp$,ck)
38000 ! 
38020   if ck=5 and exists('Q:\'&fncursys$&"mstr\Company.h"&str$(cno)) then ! cancel
38040     goto XIT
38060   else if ck=2 then 
38080     goto COMPANY_ADD
38100   else if ck=13 and (fnclient_is_converting or company_count=0) then ! import
38120     let fnchain('R:\Core\Company_Import.br')
38140   end if 
39000 ! 
39020   let cno_selected=val(resp$(1))
39040 ! 
40000   if ck=3 then ! Copy
40020     let fn_company_copy(cno_selected)
40040 !   goto MENU1
40042   else if ck=14 then ! Delete Company
40043     gosub SELECT_COMPANY
40044     let fn_company_configure(scno)
40060   else if ck=4 and company_count=>2 then ! Delete Company
40080     let fn_company_delete(cno_selected)
40100 !   goto MENU1
40120   else if ck=10 then ! Select that Company
40140     gosub SELECT_COMPANY
40160     chain program$
41000   else if ck>1000 then ! Select System
41020     let cursys$=client_has$(ck-1000)
41040     let fnreg_write(session$&'.CurSys',cursys$)
41060     let fncursys$(cursys$)
41080     let fn_setup_on_cursys_change
41100     chain program$
41120   else if ck=15 then ! SAVE
41140     gosub SELECT_COMPANY
41160     goto XIT
41180   end if 
41200   goto MENU1 ! /r
42000 SELECT_COMPANY: ! r:
42020   let fnputcno(cno_selected)
42040 ! let fnputcnam(fn_cname_of_cno$(cno_selected))
42060 ! let fncheckfileversion ! <-- no longer necessary because the core\menu handles it
42080   return  ! /r
43000 ! ______________________________________________________________________
44000 COMPANY_ADD: ! r:
44020   let fntos(sn$='selcno-'&fncursys$&'-2')
44040   let mylen=25 : let mypos=mylen+2
44060   let respc=0
44080   let fnlbl(1,1,"Company Number to add:",mylen,1)
44100   let fntxt(1,mypos,5,0,0,"30")
44120   let resp$(respc+=1)="0"
44140   let fncmdset(2)
44160   let fnacs(sn$,0,mat resp$,ck)
44180   if ck=5 then goto MENU1
44200   let cno_selected=val(resp$(1))
44220   if fn_company_already_exists(cno_selected)=1 then goto MENU1
45000   let fnputcno(cno_selected) ! let fnputcnam(empty$)
45020   let fncheckfileversion
45040   if fncursys$='PR' or fncursys$='SU' or fncursys$='TM' or fncursys$='CL' then ! no AddCNo necessary - just copy in from *.h99999 and go straight to Company Information
45100     let fncopy('R:\acs'&fncursys$&'\mstr\*.h99999 ','Q:\'&fncursys$&'mstr\*.h'&str$(cno_selected))
45120     if fncursys$='CL' then ! r: special processing for CL
45140       mat ml$(5)
45160       let ml$(1)='Would you like to import data from an old'
45180       let ml$(2)='ACS Accounts Payable system?'
45200       let ml$(3)='This is only chance.'
45220       let fnmsgbox(mat ml$,resp$,cap$,36)
45240       if resp$='Yes' then let fnchain("R:\acsCL\Conversion\APmstr-Cnv")
45260     end if  ! /r
45280     let fnchain("R:\acs"&fncursys$&"\Company")
45300   else 
45320     let fnchain(txt$="R:\acs"&fncursys$&"\AddCNo")
45340   end if 
45360 ! /r
46000 XIT: let fnxit
47000 IGNORE: continue 
48000 ! <Updateable Region: ERTN>
48020 ERTN: let fnerror(cap$,err,line,act$,"xit")
48040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
48060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
48080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
48100 ERTN_EXEC_ACT: execute act$ : goto ERTN
48120 ! /region
50000   def fn_company_already_exists(cae_cno)
50020     let cae_return=0
50040     if exists('Q:\'&fncursys$&"mstr\Company.h"&str$(cae_cno)) then 
50060       let cae_return=1
50080       mat mg$(3)
50100       let mg$(1)="Company number "&str$(cae_cno)&". "&fn_cname_of_cno$(cae_cno)&" already exist!"
50120       let mg$(2)="Select a different company number"
50140       let mg$(3)="or exit and erase the existing company first."
50160       let fnmsgbox(mat mg$,response$,cap$,16)
50180     end if 
50200     let fn_company_already_exists=cae_return
50220   fnend 
52000   def fn_cname_of_cno$*40(cno)
52020     dim coc_return$*40
52040     let coc_return$=''
52060     open #h_tmp:=fngethandle: "Name=Q:\"&fncursys$&"mstr\Company.h"&str$(cno),internal,input ioerr COC_FINIS
52080     read #h_tmp,using "Form pos 1,c 40": coc_return$
52100     close #h_tmp: 
52120 COC_FINIS: ! 
52140     let fn_cname_of_cno$=coc_return$
52160   fnend 
62000   def fn_company_delete(cno)
62020     let fntos(sn$="company_delete")
62040     let fnlbl(1,1,"**** WARNING ****",40,1,5)
62060     let fnlbl(3,1,"You have chosen to completely delete company:",60,2)
62080     let fnlbl(4,1,str$(cno)&". "&fn_cname_of_cno$(cno),60,2)
62100     let fnlbl(6,1,"The only way to get it back will be to restore a backup.",60,2)
62140     let fnlbl(9,11,"Enter ERASE to continue:",24,1)
62160     let fntxt(9,36,5)
62180     let resp$(1)=""
62200     let fncmdset(2)
62220     let fnacs(sn$,0,mat resp$,ckey)
62240     let e$=uprc$(trim$(resp$(1)))
62260     if ckey<>5 then 
62280       if e$="ERASE" then 
62300         execute 'Free "Q:\'&fncursys$&"mstr"&"\*.h"&str$(cno)&'"'
62320         mat mg$(1)
62340         let mg$(1)='Company Number '&str$(cno)&' has been Deleted!'
62360         let fnmsgbox(mat mg$,resp$,cap$,0)
62380       end if 
62400     end if 
62420   fnend 
66000   def fn_company_configure(scno)
66020     let setenv("xit_override","")
66040     let setenv("xit_override","R:\Core\Programs\SelCNo")
66060     let fnchain('R:\acs'&fncursys$&'\Company.br')
66080   fnend 
72000   def fn_company_copy(scno)
72020     dim dcnam$*40
72040     let dcno=0
72050     let dcnam$=fn_cname_of_cno$(scno)
72060 CC_SCREEN1: ! 
72080     let fntos(sn$='CopyCNo3')
72100     let lc=0
72120     let mylen=29 : let mypos=mylen+2
72140     let fnlbl(lc+=1,1,"Source Company:",mylen,1)
72160     let fnlbl(lc,mypos,str$(scno)&'. '&fn_cname_of_cno$(scno),50)
72180     let lc+=1
72200     let fnlbl(lc+=1,1,"&Destination Company Number:",mylen,1)
72220     let fntxt(lc,mypos,5,0,0,'30')
72240     let resp$(1)=str$(dcno)
72260     let fnlbl(lc+=1,1,"Destination Company Name:",mylen,1)
72280     let fntxt(lc,mypos,40,40)
72300     let resp$(2)=dcnam$
72320     let lc+=1
72340     let fnlbl(lc+=1,1,"Warning",80,2,1)
72360     let fnlbl(lc+=1,1,"Please make sure no one else is",80,2)
72380     let fnlbl(lc+=1,1,"using either company number.",80,2)
72400     let fnlbl(lc+=1,1,"If the destination company exists",80,2)
72420     let fnlbl(lc+=1,1,"it will be overwritten!",80,2)
72440     let fncmdset(2)
72460     let fnacs(sn$,0,mat resp$,ck)
72480     if ck<>5 then 
72500       let dcno=val(resp$(1))
72520       let dcnam$=resp$(2)
72540       if fn_company_already_exists(dcno)=1 then goto CC_SCREEN1
72560       execute "Copy Q:\"&fncursys$&"mstr"&"\*.h"&str$(scno)&" Q:\"&fncursys$&"mstr"&"\*.h"&str$(dcno)&" -n"
72580       execute "Copy "&"acs"&fncursys$&"\*.h"&str$(scno)&" "&"acs"&fncursys$&"\*.h"&str$(dcno)&" -n" ioerr ignore
72600       if uprc$(fncursys$)=uprc$("UB") then 
72620         execute "Copy Q:\UBmstr\ubData\*.h"&str$(scno)&" Q:\UBmstr\ubData\*.h"&str$(dcno)&" -n" ioerr ignore
72640       end if 
72660       let fn_update_company_name(dcno,dcnam$)
72680     end if 
72700   fnend 
74000   def fn_update_company_name(cno,cnam$*40)
74020     let cnam$=rtrm$(cnam$)
74040     if cnam$<>'' then 
74060       open #h_company:=fngethandle: 'Name=Q:\'&fncursys$&'mstr\Company.h'&str$(cno),internal,outin,relative 
74080       rewrite #h_company,using 'form pos 1,c 40',rec=1: cnam$
74100       close #h_company: 
74120     end if 
74140   fnend 
76000   def fn_setup
76020     if setup<>1 then 
76040       let setup=1
76060       library 'R:\Core\Library': fnacs,fnerror,fncno,fntos,fnchain,fnxit,fnputcno,fngetdir2,fncursys$,fnlbl,fncmdset,fntop,fntxt,fncmdkey,fncheckfileversion,fnmsgbox,fnflexadd1,fnflexinit1,fngethandle,fnclient_is_converting,fnclient_has_mat,fnclient$,fnreg_read,fnreg_write,fnbutton,fnfra,fnsystem_abbr_2_name$,fncopy
76080       on error goto ERTN
76100 ! ______________________________________________________________________
76120       dim cap$*128,filename$(999)*40
76140       dim resp$(10)*50,txt$*40 ! ,temp$*256
76160       dim mg$(3)*128
76180 ! r: NOTE: Regardless of which ACS System:
76200 !  * Add Company program must be called acs[cursys]\ADDCNO.br
76220 !  * Company files must be named Company.hxx
76240 ! /r
76260     end if 
76280 ! 
76300 ! r: constants and variables setup for the company grid
76320     mat colhdr$(2)
76340     let colhdr$(1)='Number'
76360     let colhdr$(2)='Name'
76380     mat colmask$(2)
76400     let colmask$(1)="30"
76420     let colmask$(2)=""
76440     dim item$(2)*40
76460 ! /r
76480     let fn_system_setup
76500   fnend 
78000   def fn_system_setup
78020     if ~fnclient_has_mat(mat client_has$) and env$("ACSDeveloper")="" then 
78030       dim ml$(1)*128
78140       mat ml$(4)
78160       let ml$(1)='Client '&fnclient$&' has nothing licensed.  Please perform an update.'
78180       let ml$(2)='If you have already performed an update and are'
78200       let ml$(3)='still receiving this message contact ACS at 1-800-643-6318'
78220       let ml$(4)='Perform an Update now?'
78240       let fnmsgbox(mat ml$,resp$,cap$,16+4)
78260       if uprc$(resp$)=uprc$("Yes") then 
78280         chain 'R:\Core\Programs\Update'
78300       else 
78320         goto XIT
78340       end if 
78360     end if 
78380 ! 
78400     dim cursys$*40,udf$*256
78420 !  let cursys$=fncursys$
78440     let fnreg_read(session$&'.CurSys',cursys$)
78480     let cursys$=fncursys$(cursys$)
78500     let fn_setup_on_cursys_change
78520   fnend 
80000   def fn_setup_on_cursys_change
80020     dim cnam$*80
80040     let fncno(cno,cnam$)
80060     if cno=0 then 
80080       let cno=1
80100       let fnputcno(cno)
80120       let fncno(cno,cnam$)
80140     end if 
80160 ! 
80180     if ~exists('Q:\'&cursys$&'mstr') then execute 'mkdir Q:\'&cursys$&'mstr'
80200     if ~exists('Q:\ini\acs'&cursys$) then execute 'mkdir Q:\ini\acs'&cursys$
80220   fnend 
