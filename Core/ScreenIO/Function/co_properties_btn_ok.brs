def fnco_properties_btn_ok
  ! library 'S:\Core\Library': fnIniSet,fnIniWrite
  library 'S:\Core\Library': fnwriteProgramPrintProperty
  ! r: if Landscape/Portrait, than switch height and width if necessary
    tmp_height=val(s$(sio_txtHeight))
    tmp_width=val(s$(sio_txtWidth))
    if s$(sio_cmbOrientation)='Landscape' then 
      if tmp_height>tmp_width then ! and it's taller than it is wide
        tmp_hold=tmp_height
        tmp_height=tmp_width : s$(sio_txtHeight)=str$(tmp_height)
        tmp_width=tmp_hold : s$(sio_txtWidth)=str$(tmp_width)
      end if 
    else if s$(sio_cmbOrientation)='Portrait' then
      if tmp_width>tmp_height then ! and it's wider than it is tall
        tmp_hold=tmp_height
        tmp_height=tmp_width : s$(sio_txtHeight)=str$(tmp_height)
        tmp_width=tmp_hold : s$(sio_txtWidth)=str$(tmp_width)
      end if 
    end if 
  ! /r
  fnwriteProgramPrintProperty('Orientation',     s$(sio_cmbOrientation)    )
  fnwriteProgramPrintProperty('Height',          s$(sio_txtHeight)         )
  fnwriteProgramPrintProperty('Width',           s$(sio_txtWidth)          )
  fnwriteProgramPrintProperty('Lines',           s$(sio_txtLpp)            )
  fnwriteProgramPrintProperty('FontSize',        s$(sio_txtFontSize)       )
  fnwriteProgramPrintProperty('TopMargin',       s$(sio_txtMarginTop)      )
  fnwriteProgramPrintProperty('BottomMargin',    s$(sio_txtMarginBottom)   )
  fnwriteProgramPrintProperty('LeftMargin',      s$(sio_txtMarginLeft)     )
  fnwriteProgramPrintProperty('RightMargin',     s$(sio_txtMarginRight)    )
  ExitMode=QuitOnly
fnend
