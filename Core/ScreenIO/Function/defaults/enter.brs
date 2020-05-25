def fnEnterDefault
  library 'S:\Core\Library': fnTop,fncompany_name,fnprogram_properties,fnBackgroundDisable
  if screenio$(si_caption)='Properties' then
     ! fnBackgroundDisable(1)
  else
    fnTop(program$,screenio$(si_caption))
    fncompany_name(0,115)
    for attrItem=1 to udim(mat attr$)
      if lwrc$(attr$(attrItem))=lwrc$('[buttons]') then
        pr #0, fields "1,5,P 1/2,[buttons],"&str$(returnkey): "S:\Core\Icon\forward-icon.png" ioerr ignore
      else if lwrc$(attr$(attrItem))=lwrc$('[buttoncancel]') then
        if env$('tmp_acs_back_arrow')<>'' then 
          pr #0, fields "1,2,P 1/2,[buttons],"&str$(returnkey): env$('tmp_acs_back_arrow') ioerr ignore
        else 
          pr #0, fields "1,2,P 1/2,[buttons],"&str$(returnkey): "S:\Core\Icon\back-icon.png" ioerr ignore
        end if 
      end if
    nex attrItem
  end if
fnend
