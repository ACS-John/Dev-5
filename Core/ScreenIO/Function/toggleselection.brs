 ! function\toggleselection.brs
 ! Created on 04/13/2009
 !
 ! fnToggleSelection - This Function toggles the current selection
 !
 !
 def fnToggleSelection(CurrentKey$,&RepopulateListviews;___,Index,I)
    index=srch(mat MarkedRecords$,CurrentKey$)
    if Index>0 then
       for I = Index to udim(MarkedRecords$)-1
          markedRecords$(I)=MarkedRecords$(I+1)
       next I
       mat MarkedRecords$(udim(MarkedRecords$)-1)
    else
       mat MarkedRecords$(udim(MarkedRecords$)+1)
       markedRecords$(udim(MarkedRecords$))=CurrentKey$
    end if
    repopulateListviews=1
 fnend