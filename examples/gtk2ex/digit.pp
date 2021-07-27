unit digit;

{$mode objfpc}
{$H+}

Interface

uses gtk,gdk,glib;

Type
  TPoint = Record
    X,Y : gint;
    end;

  PGtkDigit = ^TGtkDigit;
  TGtkDigit = Record
    ParentWidget : TGtkWidget;
    borderwidth,
    digit : guint;
    Corners : Array [1..6] of TPoint;
  end;

  PGtkDigitClass = ^TGtkDigitClass;
  TGtkDigitClass = Record
    Parent_Class : TGtkWidgetClass;
  end;

Function  GtkDigit_get_type : Guint;cdecl;
Function  GtkDigit_new : PGtkWidget;cdecl;
Procedure GtkDigit_set_digit (Obj : PGtkDigit; Digit : guint);cdecl;
Function  GtkDigit_get_digit (Obj : PGtkDigit) : guint;cdecl;

Type
  PGtkActiveDigit = ^TGtkActiveDigit;
  TGtkActiveDigit = Record
    ParentWidget : TGtkDigit;
    Button : guint8;
  end;

  PGtkActiveDigitClass = ^TGtkActiveDigitClass;
  TGtkActiveDigitClass = Record
    Parent_Class : TGtkDigitClass;
  end;

Function  GtkActiveDigit_get_type : Guint;cdecl;
Function  GtkActiveDigit_new : PGtkWidget;cdecl;

Implementation

Type
  TLEDSegment = (lsTop,lsCenter,lsBottom,
                 lsLeftTop,lsRightTop,
                 lsLeftBottom, lsRightBottom);
  TLedSegments = Array[TLedSegment] of boolean;
  TSegmentCorners = Array [1..2] of Byte;

Const
  DigitSegments : Array[0..9] of TLEDSegments =
    (
     (true,false,true,true,true,true,true),       // 0
     (false,false,false,false,true,false,true),   // 1
     (true,true,true,false,true,true,false),      // 2
     (true,true,true,false,true,false,true),      // 3
     (false,true,false,true,true,false,true),     // 4
     (true,true,true,true,false,false,true),      // 5
     (true,true,true,true,false,true,true),       // 6
     (true,false,false,false,true,false,true),    // 7
     (true,true,true,true,true,true,true),        // 8
     (true,true,true,true,true,false,true)        // 9
    );

  SegmentCorners : Array [TLEDSegment] of TSegmentCorners =
    (
     (1,2),
     (3,4),
     (5,6),
     (1,3),
     (2,4),
     (3,5),
     (4,6)
    );

Const
  GtkDigitType : guint = 0;

Procedure GTKDigitSizeRequest (Widget : PGtkWidget;
                               Request : PGtkRequisition);cdecl;

Var BW : guint;

begin
  With PGTKDigit(Widget)^ do
    BW:=BorderWidth;
  With Request^ do
    begin
    Width:=20+2*BW;
    Height:=40+2*BW;
    end;
end;

Function GTKDigitExpose (Widget : PGTKWidget;
                         ExposeEvent : PGDKEventExpose) : gint;cdecl;


Var
  Segment : TLedSegment;

begin
  With PGTKDigit(Widget)^ do
    For Segment:=lsTop to lsRightBottom do
      if DigitSegments[Digit][Segment] then
        gdk_draw_line(widget^.window,
                  PgtkStyle(widget^.thestyle)^.fg_gc[widget^.state],
                  Corners[SegmentCorners[Segment][1]].X,
                  Corners[SegmentCorners[Segment][1]].Y,
                  Corners[SegmentCorners[Segment][2]].X,
                  Corners[SegmentCorners[Segment][2]].Y
                  )
      else
        gdk_draw_line(widget^.window,
                  PgtkStyle(widget^.thestyle)^.bg_gc[widget^.state],
                  Corners[SegmentCorners[Segment][1]].X,
                  Corners[SegmentCorners[Segment][1]].Y,
                  Corners[SegmentCorners[Segment][2]].X,
                  Corners[SegmentCorners[Segment][2]].Y
                  );

end;

Procedure SetDigitCorners(Digit : PGtkDigit; IgnoreOffset : Boolean);

Var
  BW : guint;
  W,H,SX,SY : gint;
  i : longint;
  Widget : PGTKWidget;

begin
  Widget:=PGTKWidget(Digit);
  BW:=Digit^.Borderwidth;
  If IgnoreOffset then
    begin
    SX:=0;
    SY:=0;
    end
  else
    begin
    SX:=Widget^.Allocation.x;
    SY:=Widget^.Allocation.y;
    end;
  W:=Widget^.Allocation.Width-2*BW;
  H:=(Widget^.Allocation.Height-2*BW) div 2;
  With PGTKDigit(Widget)^ do
    For I:=1 to 6 do
      begin
      // Set X
      Case I of
        1,3,5 : Corners[i].X:=SX+BW;
        2,4,6 : Corners[i].X:=SX+BW+W;
      end;
      // Set Y
      Case I of
        1,2 : Corners[i].Y:=SY+BW;
        3,4 : Corners[i].Y:=SY+BW+H;
        5,6 : Corners[i].Y:=SY+BW+2*H
      end;
      end;
end;

procedure GTKDigitSizeAllocate(Widget : PGTKWidget;
                               Allocation : PGTKAllocation);cdecl;

begin
  Widget^.Allocation:=Allocation^;
  SetDigitCorners(PGtkDigit(Widget),False);
end;

Procedure GtkDigitClassInit (CObj : PGtkDigitClass);cdecl;

begin
  With PGtkWidgetClass(Cobj)^ do
    begin
    size_request:=@GTKDigitSizeRequest;
    expose_event:=@GTKDigitExpose;
    size_allocate:=@GTKDigitSizeAllocate;
    end;
end;


Procedure GtkDigitInit (Obj : PGtkDigit);cdecl;

Var I : longint;

begin
  gtk_widget_set_flags(pgtkWidget(obj),GTK_NO_WINDOW);
  With Obj^ do
    begin
    Digit:=0;
    BorderWidth:=2;
    For I:=1 to 6 do
    with Corners[i] do
      begin
      X:=0;
      Y:=0;
      end;
    end;
end;

Function GtkDigit_get_type : Guint;cdecl;

Const
  GtkDigitInfo : TGtkTypeInfo =
    (type_name : 'GtkDigit';
     object_size : SizeOf(TGtkDigit);
     class_size : SizeOf(TGtkDigitClass);
     class_init_func : TGtkClassInitFunc(@GtkDigitClassInit);
     object_init_func : TGtkObjectInitFunc(@GtkDigitInit);
     reserved_1 : Nil;
     reserved_2 : Nil;
     base_class_init_func : Nil
    );

begin
  if (GtkDigitType=0) then
    GtkDigitType:=gtk_type_unique(gtk_widget_get_type,@GtkDigitInfo);
  Result:=GtkDigitType;
end;

Function GtkDigit_new : PGtkWidget;cdecl;

begin
  Result:=gtk_type_new(GtkDigit_get_type)
end;

Procedure GtkDigit_set_digit (Obj : PGtkDigit; Digit : guint);cdecl;

begin
  if Digit in [0..9] then
    begin
    Obj^.Digit:=Digit;
    gtk_widget_draw(PGTKWidget(Obj),Nil);
    end;
end;

Function GtkDigit_get_digit (Obj : PGtkDigit) : guint;cdecl;

begin
  Result:=Obj^.Digit;
end;

{ ---------------------------------------------------------------------
    GTKActiveDigit
  ---------------------------------------------------------------------}

Procedure GtkActiveDigitRealize(widget : PgtkWidget);cdecl;

Var
 attr : TGDKWindowAttr;
 Mask : gint;

begin
  GTK_WIDGET_SET_FLAGS(widget,GTK_REALIZED);
  With Attr do
    begin
    x := widget^.allocation.x;
    y := widget^.allocation.y;
    width:=widget^.allocation.width;
    height:=widget^.allocation.height;
    wclass:=GDK_INPUT_OUTPUT;
    window_type:=gdk_window_child;
    event_mask:=gtk_widget_get_events(widget) or GDK_EXPOSURE_MASK or
                GDK_BUTTON_PRESS_MASK OR GDK_BUTTON_RELEASE_MASK;
    visual:=gtk_widget_get_visual(widget);
    colormap:=gtk_widget_get_colormap(widget);
    end;
  Mask:=GDK_WA_X or GDK_WA_Y or GDK_WA_VISUAL or GDK_WA_COLORMAP;
  widget^.Window:=gdk_window_new(widget^.parent^.window,@attr,mask);
  widget^.thestyle:=gtk_style_attach(widget^.thestyle,widget^.window);
  gdk_window_set_user_data(widget^.window,widget);
  gtk_style_set_background(widget^.thestyle,widget^.window,GTK_STATE_ACTIVE);
end;

procedure GTKActiveDigitSizeAllocate(Widget : PGTKWidget;
                               Allocation : PGTKAllocation);cdecl;
begin
  Widget^.allocation:=Allocation^;
  if GTK_WIDGET_REALIZED(widget) then
    gdk_window_move_resize(widget^.window,
                           Allocation^.x,
                           Allocation^.y,
                           Allocation^.width,
                           Allocation^.height);
  SetDigitCorners(PGTKDigit(Widget),True);
end;

Function GtkActiveDigitButtonPress(Widget: PGtKWidget;
                                    Event : PGdkEventButton) : gint;cdecl;

begin
  PGTKActiveDigit(Widget)^.Button:=Event^.Button;
end;

Function GtkActiveDigitButtonRelease(Widget: PGtKWidget;
                                      Event : PGdkEventButton) : gint;cdecl;

Var
  Digit : PGtkDigit;
  D : guint;

begin
  Digit:=PGTKDigit(Widget);
  D:=gtkdigit_get_digit(Digit);
  If PGTKActiveDigit(Digit)^.Button=Event^.Button then
    begin
    If Event^.Button=1 then
      GTKDigit_set_digit(Digit,D+1)
    else if Event^.Button=3 then
      GTKDigit_set_digit(Digit,D-1)
    else
      GTKDigit_set_digit(Digit,0);
    end;
  PGTKActiveDigit(Digit)^.Button:=0;
end;

Procedure GtkActiveDigitClassInit (CObj : PGtkActiveDigitClass);cdecl;

begin
  With PGtkWidgetClass(Cobj)^ do
    begin
    realize := @GtkActiveDigitRealize;
    size_allocate := @GtkActiveDigitSizeAllocate;
    button_press_event:=@GtkActiveDigitButtonPress;
    button_release_event:=@GtkActiveDigitButtonRelease;
    end;
end;


Procedure GtkActiveDigitInit (Obj : PGtkActiveDigit);cdecl;

Var I : longint;

begin
  gtk_widget_unset_flags(pgtkWidget(obj),GTK_NO_WINDOW);
  With Obj^ do
    Button:=0;
end;

Const
  GtkActiveDigitType : guint = 0;

Function  GtkActiveDigit_get_type : Guint;cdecl;

Const
  GtkActiveDigitInfo : TGtkTypeInfo =
    (type_name : 'GtkActiveDigit';
     object_size : SizeOf(TGtkActiveDigit);
     class_size : SizeOf(TGtkActiveDigitClass);
     class_init_func : TGtkClassInitFunc(@GtkActiveDigitClassInit);
     object_init_func : TGtkObjectInitFunc(@GtkActiveDigitInit);
     reserved_1 : Nil;
     reserved_2 : Nil;
     base_class_init_func : Nil
    );

begin
  if (GtkActiveDigitType=0) then
    GtkActiveDigitType:=gtk_type_unique(gtkdigit_get_type,@GtkActiveDigitInfo);
  Result:=GtkActiveDigitType;
end;


Function  GtkActiveDigit_new : PGtkWidget;cdecl;

begin
  Result:=gtk_type_new(GtkActiveDigit_get_type)
end;

end.  