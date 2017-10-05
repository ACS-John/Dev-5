00010 ! Replace S:\Core\CmbCNo.br
00020 ! screen ace combobox of available company numbers
00030 ! _______________________________________________________________________
00040   def library fncmbcno(myline,mypos; mysys$)
00050 ! _______________________________________________________________________
00060     library 'S:\Core\Library': fncno,fncursys$,fngetdir,fncomboa,fngethandle,fnerror
00070     on error goto ERTN
00080 ! _______________________________________________________________________
00090 ! the response$ for this has the company number in pos 43:47
00100 ! .! this will change with 5 digit company numbers
00110 ! _______________________________________________________________________
00120     dim opt$(9999)*48,filename$(9999)*18,cnam$*40,oldcnam$*40,resp$(10)*80,sys_data_path$*256
00130 ! 
00140     mat opt$(9999) : mat filename$(9999)
00150     fncno(cno,oldcnam$)
00170     if trim$(mysys$)='' then 
00180       sys_data_path$=env$('Q')&'\'&fncursys$&"mstr"
00190     else 
00200       sys_data_path$=env$('Q')&'\'&mysys$&"mstr"
00210     end if 
00220 ! 
00230     fngetdir(sys_data_path$,mat filename$,empty$,temp$="Company.h*")
00232     let x:=fngethandle
00240     for a=1 to udim(filename$)
00250       let filename$=trim$(filename$)
00260       if filename$(a)<>"" then 
00270         end=len(filename$(a))
00280         opt$(a)=filename$(a)(10:end)
00290         open #x: "Name="&sys_data_path$&"\Company.h"&opt$(a),internal,input 
00300         read #x,using "Form pos 1,c 40": cnam$
00310         close #x: 
00320         if val(opt$(a))=99999 then 
00330           kill99999=1
00340         else 
00350           kill99999=0
00360         end if 
00370         opt$(a)=cnam$&" ("&cnvrt$("pic(#####)",val(opt$(a)))&")"
00380       else 
00390         if kill99999=1 then 
00400           mat opt$(a-2)
00410         else 
00420           mat opt$(a-1)
00430         end if 
00440         goto EXITFOR
00450       end if 
00460     next a
00470 ! _______________________________________________________________________
00480 EXITFOR: ! 
00520     fncomboa('CmbCNo-'&fncursys$,myline,mypos,mat opt$,'Select from currently installed companies for the '&fncursys$&' system.',55)
00530     goto XIT
00550 ! <Updateable Region: ERTN>
00560 ERTN: fnerror(program$,err,line,act$,"xit")
00570     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00580     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00590     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00600 ERTN_EXEC_ACT: execute act$ : goto ERTN
00610 ! /region
00630 XIT: fnend 
