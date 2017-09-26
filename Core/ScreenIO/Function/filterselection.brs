 ! function\filterselection.brs
 ! Created on 07/13/2009
 !
 ! fnFilterSelection - This Function displays the current list with the
 !  selected items highlighted
 !
 !
 def fnFilterSelection$(mat f$,mat f,mat ScreenIO$;___,CurrentKey$)
    let CurrentKey$=fnBuildKey$(Trim$(Screenio$(Si_Filelay)),Mat F$,Mat F)
    if srch(mat MarkedRecords$,CurrentKey$)>0 then
       let fnFilterSelection$="/#000000:#FFFF00"
    else
       let fnFilterSelection$="/#FFFFFF:#000000"
    end if
 fnend
