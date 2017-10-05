00001 ! FileIO Library Basic Addon Template by Gabriel Bakker
00002 ! Copyright 2012 Gabriel Bakker, Sage AX
00003 ! Created: 2/6/2012
00004 ! 
00005 ! Use this template library as an example of how
00006 ! to construct other template libraries.
00007 ! 
00008 ! Every Template must have two library functions,
00009 !  fnTemplateList and fnRunTemplate.
00010 ! 
00011 !  fnTemplateList(mat TemplateDesc$)
00012 !  takes an array of descriptions and returns a list of
00013 !  the available functions in this template library.
00014 ! 
00015 !  fnRunTemplate(Template,FileLay$)
00016 !  this function takes the given template Index and
00017 !  file layout and calls your template function.
00018 ! 
00019 !  A template function should read the file layout
00020 !  and then build a string of code and return it
00021 !  in the clip board so that a programmer can
00022 !  paste it into his editor of choice later.
00023 ! 
00024 ! 
00025 ! To add to this template library, add your
00026 !  template description and template label
00027 !  to the list and gosub statement at the
00028 !  bottom of this library.
00029 ! 
00030 !  Then write some template code at that label
00031 !  in a subroutine that sets the code to return
00032 !  in the "ReturnCode$" variable, or use the
00033 !  fnReturnCode function.
00034 ! 
00035   dim keys$(1)*255,keydescription$(1)*255,ssubs$(1),nsubs$(1),sspec$(1),nspec$(1)
00036   dim sdescription$(1)*255,ndescription$(1)*255,spos(1),npos(1)
00037   dim filename$*255,prefix$
00038   dim longestelement
00039 ! 
00040   dim numberlist$
00041   dim keyfields$(1)
00042   dim keygiven
00043   dim keylength, length, sub
00044 ! 
00045   dim codeline$*400
00046 ! 
00047 ! 
00048 ! 
00049 ERTN: ! r: Template for a ACS Error Routine
00050   fnreturncode('! <updateable region: ertn>')
00051   fnreturncode('ERTN: ')
00052   fnreturncode('  fnerror(program$,err,line,act$,"xit")')
00053   fnreturncode('  if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT')
00054   fnreturncode('  if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT')
00055   fnreturncode('  pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT')
00056   fnreturncode('ERTN_EXEC_ACT: execute act$ : goto ERTN')
00057   fnreturncode('! </updateable region: ertn>')
00058   return  ! /r
00059 ! 
00060   dim returncode$*20000
00061   def fnreturncode(string$*512)
00062     returncode$=returncode$&string$&chr$(13)&chr$(10)
00063   fnend 
00064 ! 
00065   def library fnruntemplate(template,filelay$;___,index)
00066     fnestablishlinkage
00067     returncode$=""
00068     fnreadentirelayout(filelay$,filename$,prefix$,mat keys$,mat keydescription$,mat ssubs$,mat nsubs$,mat sspec$,mat nspec$,mat sdescription$,mat ndescription$,mat spos,mat npos)
00069     longestelement=0
00070     for index=1 to udim(mat sspec$)
00071       longestelement=max(longestelement,fnlength(sspec$(index)))
00072     next index
00073     for index=1 to udim(mat nspec$)
00074       longestelement=max(longestelement,fnlength(nspec$(index)))
00075     next index
00076     for index=1 to udim(mat ssubs$)
00077       ssubs$(index)=lwrc$(ssubs$(index))
00078     next index
00079     for index=1 to udim(mat nsubs$)
00080       nsubs$(index)=lwrc$(nsubs$(index))
00081     next index
00082     prefix$=trim$(prefix$)
00083     gosub RUNTEMPLATE
00084 ! 
00085     if len(returncode$) then let setenv("CLIPBOARD",returncode$)
00086   fnend 
00087 ! 
00088 RUNTEMPLATE: ! Template List gosub statement:
00089   on template gosub ERTN
00090   return 
00091 ! 
00092 TEMPLATELIST: ! Dictionary listing of available functions
00093   data "ERtn"
00094 ! 
00095   def library fntemplatelist(mat templatedesc$)
00096     restore TEMPLATELIST
00097     mat templatedesc$(1)
00098     read mat templatedesc$
00099   fnend 
00100 ! 
00101   dim linkageestablished
00102   def fnestablishlinkage
00103     if ~linkageestablished then 
00104       library "S:\Core\FileIO\fileio" : fnopenfile, fnclose, fnreadentirelayout, fngetfilenumber, fngetkeyelements, fnlength, fnaskcombo$
00105       linkageestablished=1
00106     end if 
00107   fnend 
00108 ! 
99000 ! #Autonumber# 99000,10
99010 OPEN: ! ***** Function To Call Library Openfile And Proc Subs
99020   def fnopen(filename$*255, mat f$, mat f, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,supress_prompt,ignore_errors,___,index)
99030     dim _fileiosubs$(1)*800, _loadedsubs$(1)*80
99040     fnopen=fnopenfile(filename$, mat f$, mat f, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$, supress_prompt,ignore_errors,program$)
99050     if srch(_loadedsubs$,uprc$(filename$))<=0 then : mat _loadedsubs$(udim(_loadedsubs$)+1) : let _loadedsubs$(udim(_loadedsubs$))=uprc$(filename$) : for index=1 to udim(mat _fileiosubs$) : execute (_fileiosubs$(index)) : next index
99060   fnend 
99070 ! 
99080 IGNORE: continue 
