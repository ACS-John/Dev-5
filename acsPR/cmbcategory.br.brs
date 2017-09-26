00010 ! Replace S:\acsPR\CmbCategory.br
00020 ! creates a screen ace combobox for Category
12000 def library fncmbcategory(myline,mypos; addall,container,indexfile$*200)
12020   ! ____________________________________________________________________
12040   library 'S:\Core\Library': fncombof
12060   dim df$*200,if$*200
12080   ! ____________________________________________________________________
12100   if addall<>1 then let addall=0
12120   if addall=0 then 
12140     let fen$="Ccategory.h"&env$('cno') 
12160   else 
12180     let fen$="CcategoryALL.h"&env$('cno')
12200   end if 
12220   if indexfile$="" then 
12240     let if$=env$('Q')&"\PRmstr\categoryidx.h"&env$('cno') 
12260   else 
12280     let if$=indexfile$
12300   end if
12320   let fncombof(fen$,myline,mypos,43,env$('Q')&"\PRmstr\category.h"&env$('cno'),1,5,6,30,if$,1+addall,0,"Select from the list of Category records. To add a Category record, take the Add option.",container)
12340   let indexfile$=""
12360 fnend 
