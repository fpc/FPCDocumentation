unit fileedit;

{$mode objfpc}
{$H+}

Interface

uses gtk,gdk,glib;

Type
  PGtkFileEdit = ^TGtkFileEdit;
  TGtkFileEdit = Record
    Box : TGtkHBox;
    Edit : PGtkEntry;
    Button : PGtkButton;
    Image : PGtkPixmap;
    Dialog : PGtkFileSelection;
  end;

  PGtkFileEditClass = ^TGtkFileEditClass;
  TGtkFileEditClass = Record
    Parent_Class : TgtkHBoxClass;
    DefaultPixmap : PGdkPixmap;
    DefaultBitMap : PGdkBitmap;
  end;

Function  GtkFileEdit_get_type : Guint;cdecl;
Function  GtkFileEdit_new : PGtkWidget;cdecl;
Procedure GtkFileEdit_set_filename (Obj : PGtkFileEdit; FileName : String);cdecl;
Function  GtkFileEdit_get_filename (Obj : PGtkFileEdit) : String;cdecl;

Implementation

Const
  GtkFileEditType : guint = 0;

Procedure GtkFileEditClassInit (CObj : PGtkFileEditClass);cdecl;

begin
  With Cobj^ do
    DefaultPixMap:=gdk_pixmap_create_from_xpm(Nil,@DefaultBitmap,
                                              Nil,'fileopen.xpm');
end;

procedure GtkFileEditButtonClick (Obj : PGtkObject; Data : PGtkFileEdit);cdecl;forward;


Procedure GtkFileEditInit (Obj : PGtkFileEdit);cdecl;

Var
  PClass : PGtkFileEditClass;

begin
  PClass:=PGtkFileEditClass(PGtkObject(Obj)^.klass);
  With Obj^ do
    begin
    Edit := PgtkEntry(gtk_entry_new);
    Button := PgtkButton(gtk_button_new);
    Image := PgtkPixMap(gtk_pixmap_new(PClass^.DefaultPixmap,
                                       PClass^.DefaultBitmap));
    gtk_container_add(PGtkContainer(Button),PGtkWidget(Image));
    gtk_box_pack_start(PgtkBox(Obj),PGtkWidget(Edit),True,True,0);
    gtk_box_pack_start(PgtkBox(Obj),PGtkWidget(Button),False,True,0);
    gtk_signal_connect(PgtkObject(Button),'clicked',
                       TGtkSignalFunc(@GtkFileEditButtonClick),Obj);
    end;
  gtk_widget_show_all(PGtkWidget(Obj));
end;

Function GtkFileEdit_get_type : Guint;cdecl;

Const
  GtkFileEditInfo : TGtkTypeInfo =
    (type_name : 'GtkFileEdit';
     object_size : SizeOf(TGtkFileEdit);
     class_size : SizeOf(TGtkFileEditClass);
     class_init_func : TGtkClassInitFunc(@GtkFileEditClassInit);
     object_init_func : TGtkObjectInitFunc(@GtkFileEditInit);
     reserved_1 : Nil;
     reserved_2 : Nil;
     base_class_init_func : Nil
    );

begin
  if (GtkFileEditType=0) then
    GtkFileEditType:=gtk_type_unique(gtk_hbox_get_type,@GtkFileEditInfo);
  Result:=GtkFileEditType;
end;

Function GtkFileEdit_new : PGtkWidget;cdecl;

begin
  Result:=gtk_type_new(GtkFIleEdit_get_type)
end;

Procedure GtkFileEdit_set_filename (Obj : PGtkFileEdit; FileName : String);cdecl;

begin
  gtk_entry_set_text(Obj^.Edit,PChar(FileName));
end;

Function GtkFileEdit_get_filename (Obj : PGtkFileEdit) : String;cdecl;

begin
  Result:=StrPas(gtk_entry_get_text(Obj^.Edit));
end;

Procedure GtkStoreFileName(Button : PgtkButton;
                           TheRec : PGtkFileEdit); cdecl;

begin
  With TheRec^ do
    begin
    gtk_entry_set_text(Edit,gtk_file_selection_get_filename(Dialog));
    dialog:=Nil;
    end;
end;

Procedure GtkFileEditButtonClick (Obj : PGtkObject; Data : PgtkFileEdit);cdecl;

Var
  Dialog : PGtkFileSelection;

begin
  Dialog := PGtkFileSelection(gtk_file_selection_new('Please select a file'));
  Data^.Dialog:=Dialog;
  gtk_signal_connect(PGTKObject(Dialog^.ok_button),'clicked',
                     TGTKSignalFunc(@GtkStoreFileName),data);
  gtk_signal_connect_object (PGtkObject((Dialog)^.ok_button),'clicked',
                            TGTKSIGNALFUNC (@gtk_widget_destroy), PgtkObject(Dialog));
  gtk_signal_connect_object (PGtkObject((Dialog)^.cancel_button),'clicked',
                            TGTKSIGNALFUNC (@gtk_widget_destroy), PgtkObject(Dialog));
  gtk_widget_show(PgtkWidget(dialog));
end;

end.  