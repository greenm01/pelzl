# 0 "_curses.ml"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3
# 0 "<command-line>" 2
# 1 "_curses.ml"
(*pp gcc -x c -E *)

type window
type screen
type terminal
type chtype = int
type attr_t = int
type err = bool
# 62 "_curses.ml"
module Acs = struct
  type acs = {
    ulcorner: chtype;
    llcorner: chtype;
    urcorner: chtype;
    lrcorner: chtype;
    ltee: chtype;
    rtee: chtype;
    btee: chtype;
    ttee: chtype;
    hline: chtype;
    vline: chtype;
    plus: chtype;
    s1: chtype;
    s9: chtype;
    diamond: chtype;
    ckboard: chtype;
    degree: chtype;
    plminus: chtype;
    bullet: chtype;
    larrow: chtype;
    rarrow: chtype;
    darrow: chtype;
    uarrow: chtype;
    board: chtype;
    lantern: chtype;
    block: chtype;
    s3: chtype;
    s7: chtype;
    lequal: chtype;
    gequal: chtype;
    pi: chtype;
    nequal: chtype;
    sterling: chtype
  }
  let bssb a = a.ulcorner
  let ssbb a = a.llcorner
  let bbss a = a.urcorner
  let sbbs a = a.lrcorner
  let sbss a = a.rtee
  let sssb a = a.ltee
  let ssbs a = a.btee
  let bsss a = a.ttee
  let bsbs a = a.hline
  let sbsb a = a.vline
  let ssss a = a.plus
end


# 1 "_functions.c" 1



external addch : chtype -> err = "mlcurses_addch"
external waddch : window -> chtype -> err = "mlcurses_waddch"
external mvaddch : int -> int -> chtype -> err = "mlcurses_mvaddch"
external mvwaddch : window -> int -> int -> chtype -> err = "mlcurses_mvwaddch"
external echochar : chtype -> err = "mlcurses_echochar"
external wechochar : window -> chtype -> err = "mlcurses_wechochar"
# 19 "_functions.c"
external addchstr : chtype array -> err = "mlcurses_addchstr"
(* int i,c=Wosize_val(aa),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(aa,i+0)); t[i]=0; r=addchstr(t); free(t); r_err(r); *)
external waddchstr : window -> chtype array -> err = "mlcurses_waddchstr"
(* int i,c=Wosize_val(ab),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(ab,i+0)); t[i]=0; r=waddchstr(a_window(aa),t); free(t); r_err(r); *)
external mvaddchstr : int -> int -> chtype array -> err = "mlcurses_mvaddchstr"
(* int i,c=Wosize_val(ac),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(ac,i+0)); t[i]=0; r=mvaddchstr(a_int(aa),a_int(ab),t); free(t); r_err(r); *)
external mvwaddchstr : window -> int -> int -> chtype array -> err = "mlcurses_mvwaddchstr"
(* int i,c=Wosize_val(ad),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(ad,i+0)); t[i]=0;
  r=mvwaddchstr(a_window(aa),a_int(ab),a_int(ac),t); free(t); r_err(r); *)
external addchnstr : chtype array -> int -> int -> err = "mlcurses_addchnstr"
(* int i0=Int_val(ab);
  int i,c=Int_val(ac),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(aa,i+i0)); t[i]=0;
  r=addchnstr(t,Int_val(ac)); free(t); r_err(r); *)
external waddchnstr : window -> chtype array -> int -> int -> err = "mlcurses_waddchnstr"
(* int i0=Int_val(ac);
  int i,c=Int_val(ad),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(ab,i+i0)); t[i]=0;
  r=waddchnstr(a_window(aa),t,Int_val(ad)); free(t); r_err(r); *)
external mvaddchnstr : int -> int -> chtype array -> int -> int -> err = "mlcurses_mvaddchnstr"
(* int i0=Int_val(ad);
  int i,c=Int_val(ae),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(ac,i+i0)); t[i]=0;
  r=mvaddchnstr(a_int(aa),a_int(ab),t,Int_val(ae)); free(t); r_err(r); *)
external mvwaddchnstr : window -> int -> int -> chtype array -> int -> int -> err = "mlcurses_mvwaddchnstr_bytecode" "mlcurses_mvwaddchnstr_native"
(* int i0=Int_val(ae);
  int i,c=Int_val(af),r; chtype *t=malloc((c+1)*sizeof(chtype)); if(t==NULL) caml_failwith("Out of memory"); for(i=0;i<c;i++) t[i]=Int_val(Field(ad,i+i0)); t[i]=0;
  r=mvwaddchnstr(a_window(aa),a_int(ab),a_int(ac),t,Int_val(af)); free(t); r_err(r); *)





external addstr : string -> err = "mlcurses_addstr"
external waddstr : window -> string -> err = "mlcurses_waddstr"
external mvaddstr : int -> int -> string -> err = "mlcurses_mvaddstr"
external mvwaddstr : window -> int -> int -> string -> err = "mlcurses_mvwaddstr"
external addnstr : string -> int -> int -> err = "mlcurses_addnstr"
(* r_err(addnstr(a_string(aa)+a_int(ab),a_int(ac))); *)
external waddnstr : window -> string -> int -> int -> err = "mlcurses_waddnstr"
(* r_err(waddnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); *)
external mvaddnstr : int -> int -> string -> int -> int -> err = "mlcurses_mvaddnstr"
(* r_err(mvaddnstr(a_int(aa),a_int(ab),
  a_string(ac)+a_int(ad),a_int(ae))); *)
external mvwaddnstr : window -> int -> int -> string -> int -> int -> err = "mlcurses_mvwaddnstr_bytecode" "mlcurses_mvwaddnstr_native"
(* r_err(mvwaddnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad)+a_int(ae),a_int(af))); *)



external attroff : int -> unit = "mlcurses_attroff"
external wattroff : window -> int -> unit = "mlcurses_wattroff"
external attron : int -> unit = "mlcurses_attron"
external wattron : window -> int -> unit = "mlcurses_wattron"
external attrset : int -> unit = "mlcurses_attrset"
external wattrset : window -> int -> unit = "mlcurses_wattrset"
external standend : unit -> unit = "mlcurses_standend"
external wstandend : window -> unit = "mlcurses_wstandend"
external standout : unit -> unit = "mlcurses_standout"
external wstandout : window -> unit = "mlcurses_wstandout"

external attr_off : attr_t -> unit = "mlcurses_attr_off"
(* attr_off(a_attr_t(aa),NULL); CAMLreturn(Val_unit); *)
external wattr_off : window -> attr_t -> unit = "mlcurses_wattr_off"
(* wattr_off(a_window(aa),a_attr_t(ab),NULL); CAMLreturn(Val_unit); *)
external attr_on : attr_t -> unit = "mlcurses_attr_on"
(* attr_on(a_attr_t(aa),NULL); CAMLreturn(Val_unit); *)
external wattr_on : window -> attr_t -> unit = "mlcurses_wattr_on"
(* wattr_on(a_window(aa),a_attr_t(ab),NULL); CAMLreturn(Val_unit); *)
external attr_set : attr_t -> int -> unit = "mlcurses_attr_set"
(* attr_set(a_attr_t(aa),a_int(ab),NULL); CAMLreturn(Val_unit); *)
external wattr_set : window -> attr_t -> int -> unit = "mlcurses_wattr_set"
(* wattr_set(a_window(aa),a_attr_t(ab),a_int(ac),NULL); CAMLreturn(Val_unit); *)
# 103 "_functions.c"
external chgat : int -> attr_t -> int -> unit = "mlcurses_chgat"
(* chgat(a_int(aa),a_attr_t(ab),a_int(ac),NULL); CAMLreturn(Val_unit); *)
external wchgat : window -> int -> attr_t -> int -> unit = "mlcurses_wchgat"
(* wchgat(a_window(aa),a_int(ab),a_attr_t(ac),a_int(ad),NULL);
  CAMLreturn(Val_unit); *)
external mvchgat : int -> int -> int -> attr_t -> int -> unit = "mlcurses_mvchgat"
(* mvchgat(a_int(aa),a_int(ab),a_int(ac),a_attr_t(ad),a_int(ae),NULL);
  CAMLreturn(Val_unit); *)
external mvwchgat : window -> int -> int -> int -> attr_t -> int -> unit = "mlcurses_mvwchgat_bytecode" "mlcurses_mvwchgat_native"
(* mvwchgat(a_window(aa),a_int(ab),a_int(ac),a_int(ad),a_attr_t(ae),
  a_int(af),NULL); CAMLreturn(Val_unit); *)



external beep : unit -> err = "mlcurses_beep"
external flash : unit -> err = "mlcurses_flash"



external bkgdset : chtype -> unit = "mlcurses_bkgdset"
external wbkgdset : window -> chtype -> unit = "mlcurses_wbkgdset"
external bkgd : chtype -> unit = "mlcurses_bkgd"
external wbkgd : window -> chtype -> unit = "mlcurses_wbkgd"
external getbkgd : window -> chtype = "mlcurses_getbkgd"



external border : chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> unit = "mlcurses_border_bytecode" "mlcurses_border_native"
external wborder : window -> chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> unit = "mlcurses_wborder_bytecode" "mlcurses_wborder_native"
external box : window -> chtype -> chtype -> unit = "mlcurses_box"
external hline : chtype -> int -> unit = "mlcurses_hline"
external whline : window -> chtype -> int -> unit = "mlcurses_whline"
external vline : chtype -> int -> unit = "mlcurses_vline"
external wvline : window -> chtype -> int -> unit = "mlcurses_wvline"

external mvhline : int -> int -> chtype -> int -> unit = "mlcurses_mvhline"
external mvwhline : window -> int -> int -> chtype -> int -> unit = "mlcurses_mvwhline"
external mvvline : int -> int -> chtype -> int -> unit = "mlcurses_mvvline"
external mvwvline : window -> int -> int -> chtype -> int -> unit = "mlcurses_mvwvline"



external erase : unit -> unit = "mlcurses_erase"
external werase : window -> unit = "mlcurses_werase"
external clear : unit -> unit = "mlcurses_clear"
external wclear : window -> unit = "mlcurses_wclear"
external clrtobot : unit -> unit = "mlcurses_clrtobot"
external wclrtobot : window -> unit = "mlcurses_wclrtobot"
external clrtoeol : unit -> unit = "mlcurses_clrtoeol"
external wclrtoeol : window -> unit = "mlcurses_wclrtoeol"



external start_color : unit -> err = "mlcurses_start_color"
external init_pair : int -> int -> int -> err = "mlcurses_init_pair"
external init_color : int -> int -> int -> int -> err = "mlcurses_init_color"
external has_colors : unit -> bool = "mlcurses_has_colors"
external can_change_color : unit -> bool = "mlcurses_can_change_color"
external color_content : int -> int*int*int = "mlcurses_color_content"
(* short x,y,z; if(color_content(Int_val(aa),&x,&y,&z)==ERR)
  x=y=z=-1; r_int_int_int(x,y,z); *)
external pair_content : int -> int*int = "mlcurses_pair_content"
(* short x,y; if(pair_content(Int_val(aa),&y,&x)==ERR) x=y=-1;
  r_int_int(x,y); *)
external colors : unit -> int = "mlcurses_colors" (* r_int(COLORS); *)
external color_pairs : unit -> int = "mlcurses_color_pairs" (* r_int(COLOR_PAIRS); *)



external use_default_colors : unit -> err = "mlcurses_use_default_colors"
external delch : unit -> err = "mlcurses_delch"
external wdelch : window -> err = "mlcurses_wdelch"
external mvdelch : int -> int -> err = "mlcurses_mvdelch"
external mvwdelch : window -> int -> int -> err = "mlcurses_mvwdelch"



external deleteln : unit -> err = "mlcurses_deleteln"
external wdeleteln : window -> err = "mlcurses_wdeleteln"
external insdelln : int -> err = "mlcurses_insdelln"
external winsdelln : window -> int -> err = "mlcurses_winsdelln"
external insertln : unit -> err = "mlcurses_insertln"
external winsertln : window -> err = "mlcurses_winsertln"



external mvgetch : int -> int -> int = "mlcurses_mvgetch"
external mvwgetch : window -> int -> int -> int = "mlcurses_mvwgetch"
external ungetch : int -> err = "mlcurses_ungetch"



external wgetstr : window -> string -> err = "mlcurses_wgetstr"
(* r_err(wgetnstr(a_window(aa),a_string(ab),caml_string_length(ab))); *)
external mvgetstr : int -> int -> string -> err = "mlcurses_mvgetstr"
(* r_err(mvgetnstr(a_int(aa),a_int(ab),a_string(ac),caml_string_length(ac))); *)
external mvwgetstr : window -> int -> int -> string -> err = "mlcurses_mvwgetstr"
(* r_err(mvwgetnstr(a_window(aa),a_int(ab),a_int(ac),a_string(ad),
  caml_string_length(ad))); *)
external wgetnstr : window -> string -> int -> int -> err = "mlcurses_wgetnstr"
(* r_err(wgetnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); *)
external mvgetnstr : int -> int -> string -> int -> int -> err = "mlcurses_mvgetnstr"
(* r_err(mvgetnstr(a_int(aa),a_int(ab),a_string(ac)+a_int(ad),
  a_int(ae))); *)
external mvwgetnstr : window -> int -> int -> string -> int -> int -> err = "mlcurses_mvwgetnstr_bytecode" "mlcurses_mvwgetnstr_native"
(* r_err(mvwgetnstr(a_window(aa),a_int(ab),a_int(ac),a_string(ad)+a_int(ae),
  a_int(af))); *)



external getyx : window -> int*int = "mlcurses_getyx"
(* int x,y; getyx(a_window(aa),y,x); r_int_int(y,x); *)
external getparyx : window -> int*int = "mlcurses_getparyx"
(* int x,y; getparyx(a_window(aa),y,x); r_int_int(y,x); *)
external getbegyx : window -> int*int = "mlcurses_getbegyx"
(* int x,y; getbegyx(a_window(aa),y,x); r_int_int(y,x); *)
external getmaxyx : window -> int*int = "mlcurses_getmaxyx"
(* int x,y; getmaxyx(a_window(aa),y,x); r_int_int(y,x); *)



external inch : unit -> chtype = "mlcurses_inch"
external winch : window -> chtype = "mlcurses_winch"
external mvinch : int -> int -> chtype = "mlcurses_mvinch"
external mvwinch : window -> int -> int -> chtype = "mlcurses_mvwinch"







external inchstr : chtype array -> err = "mlcurses_inchstr"
(* int i,ne=Wosize_val(aa); chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=inchnstr(tbl,ne);
  for(i=0;i<ne;i++) Store_field(aa,i+0,Val_int(tbl[i])); r_err(ret); *)
external winchstr : window -> chtype array -> err = "mlcurses_winchstr"
(* int i,ne=Wosize_val(ab); chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=winchnstr(a_window(aa),tbl,ne);
  for(i=0;i<ne;i++) Store_field(ab,i+0,Val_int(tbl[i])); r_err(ret); *)
external mvinchstr : int -> int -> chtype array -> err = "mlcurses_mvinchstr"
(* int i,ne=Wosize_val(ac); chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=mvinchnstr(a_int(aa),a_int(ab),tbl,ne);
  for(i=0;i<ne;i++) Store_field(ac,i+0,Val_int(tbl[i])); r_err(ret); *)
external mvwinchstr : window -> int -> int -> chtype array -> err = "mlcurses_mvwinchstr"
(* int i,ne=Wosize_val(ad); chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=mvwinchnstr(a_window(aa),a_int(ab),a_int(ac),tbl,ne);
  for(i=0;i<ne;i++) Store_field(ad,i+0,Val_int(tbl[i])); r_err(ret); *)
external inchnstr : chtype array -> int -> int -> err = "mlcurses_inchnstr"
(* int i0=a_int(ab);
  int i,ne=Wosize_val(aa)-i0; chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=inchnstr(tbl,a_int(ac));
  for(i=0;i<ne;i++) Store_field(aa,i+i0,Val_int(tbl[i])); r_err(ret); *)
external winchnstr : window -> chtype array -> int -> int -> err = "mlcurses_winchnstr"
(* int i0=a_int(ac);
  int i,ne=Wosize_val(ab)-i0; chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=winchnstr(a_window(aa),tbl,a_int(ad));
  for(i=0;i<ne;i++) Store_field(ab,i+i0,Val_int(tbl[i])); r_err(ret); *)
external mvinchnstr : int -> int -> chtype array -> int -> int -> err = "mlcurses_mvinchnstr"
(* int i0=a_int(ad);
  int i,ne=Wosize_val(ac)-i0; chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=mvinchnstr(a_int(aa),a_int(ab),tbl,a_int(ae));
  for(i=0;i<ne;i++) Store_field(ac,i+i0,Val_int(tbl[i])); r_err(ret); *)
external mvwinchnstr : window -> int -> int -> chtype array -> int -> int -> err = "mlcurses_mvwinchnstr_bytecode" "mlcurses_mvwinchnstr_native"
(* int i0=a_int(ae);
  int i,ne=Wosize_val(ad)-i0; chtype *tbl=malloc((ne+1)*sizeof(chtype));
  int ret=mvwinchnstr(a_window(aa),a_int(ab),a_int(ac),tbl,a_int(af));
  for(i=0;i<ne;i++) Store_field(ad,i+i0,Val_int(tbl[i])); r_err(ret); *)





external insch : chtype -> err = "mlcurses_insch"
external winsch : window -> chtype -> err = "mlcurses_winsch"
external mvinsch : int -> int -> chtype -> err = "mlcurses_mvinsch"
external mvwinsch : window -> int -> int -> chtype -> err = "mlcurses_mvwinsch"



external initscr : unit -> window = "mlcurses_initscr"
external endwin : unit -> unit = "mlcurses_endwin"
external isendwin : unit -> bool = "mlcurses_isendwin"
external newterm : string -> Unix.file_descr -> Unix.file_descr -> screen = "mlcurses_newterm"
(*
  CAMLlocal1(r);
  int fda=dup(a_int(ab)),fdb=dup(a_int(ac));
  FILE *fa=fdopen(fda,"w"),*fb=fdopen(fdb,"r");
  SCREEN *s;
  AWB(r);
  r=caml_alloc_tuple(3);
  Store_field(r,0,caml_copy_nativeint((intnat) fa));
  Store_field(r,1,caml_copy_nativeint((intnat) fb));
  s=newterm(a_string(aa),fa,fb);
  if(s==NULL){ fclose(fa); fclose(fb); caml_failwith("newterm"); }
  Store_field(r,2,Val_screen(s));
  CAMLreturn(r); *)
external set_term : screen -> unit = "mlcurses_set_term"
external delscreen : screen -> unit = "mlcurses_delscreen"
(* delscreen(a_screen(aa));
  fclose((FILE * )Nativeint_val(Field(aa,0)));
  fclose((FILE * )Nativeint_val(Field(aa,1)));
  CAMLreturn(Val_unit); *)
external stdscr : unit -> window = "mlcurses_stdscr"
(* r_window(stdscr); *)
external null_window : unit -> window = "mlcurses_null_window" (* r_window(NULL); *)



external insstr : string -> err = "mlcurses_insstr"
(* r_err(insnstr(a_string(aa),caml_string_length(aa))); *)
external winsstr : window -> string -> err = "mlcurses_winsstr"
(* r_err(winsnstr(a_window(aa),a_string(ab),caml_string_length(ab))); *)
external mvinsstr : int -> int -> string -> err = "mlcurses_mvinsstr"
(* r_err(mvinsnstr(a_int(aa),a_int(ab),a_string(ac),caml_string_length(ac))); *)
external mvwinsstr : window -> int -> int -> string -> err = "mlcurses_mvwinsstr"
(* r_err(mvwinsnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad),caml_string_length(ad))); *)
external insnstr : string -> int -> int -> err = "mlcurses_insnstr"
(* r_err(insnstr(a_string(aa)+a_int(ab),a_int(ac))); *)
external winsnstr : window -> string -> int -> int -> err = "mlcurses_winsnstr"
(* r_err(winsnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); *)
external mvinsnstr : int -> int -> string -> int -> int -> err = "mlcurses_mvinsnstr"
(* r_err(mvinsnstr(a_int(aa),a_int(ab),a_string(ac)+a_int(ad),a_int(ae))); *)
external mvwinsnstr : window -> int -> int -> string -> int -> int -> err = "mlcurses_mvwinsnstr_bytecode" "mlcurses_mvwinsnstr_native"
(* r_err(mvwinsnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad)+a_int(ae),a_int(af))); *)



external instr : string -> err = "mlcurses_instr"
(* r_err(innstr(a_string(aa),caml_string_length(aa))); *)
external winstr : window -> string -> err = "mlcurses_winstr"
(* r_err(winnstr(a_window(aa),a_string(ab),caml_string_length(ab))); *)
external mvinstr : int -> int -> string -> err = "mlcurses_mvinstr"
(* r_err(mvinnstr(a_int(aa),a_int(ab),a_string(ac),caml_string_length(ac))); *)
external mvwinstr : window -> int -> int -> string -> err = "mlcurses_mvwinstr"
(* r_err(mvwinnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad),caml_string_length(ad))); *)
external innstr : string -> int -> int -> err = "mlcurses_innstr"
(* r_err(innstr(a_string(aa)+a_int(ab),a_int(ac))); *)
external winnstr : window -> string -> int -> int -> err = "mlcurses_winnstr"
(* r_err(winnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); *)
external mvinnstr : int -> int -> string -> int -> int -> err = "mlcurses_mvinnstr"
(* r_err(mvinnstr(a_int(aa),a_int(ab),a_string(ac)+a_int(ad),a_int(ae))); *)
external mvwinnstr : window -> int -> int -> string -> int -> int -> err = "mlcurses_mvwinnstr_bytecode" "mlcurses_mvwinnstr_native"
(* r_err(mvwinnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad)+a_int(ae),a_int(af))); *)



external cbreak : unit -> err = "mlcurses_cbreak"
external nocbreak : unit -> err = "mlcurses_nocbreak"
external echo : unit -> err = "mlcurses_echo"
external noecho : unit -> err = "mlcurses_noecho"
external halfdelay : int -> err = "mlcurses_halfdelay"
external intrflush : window -> bool -> err = "mlcurses_intrflush"
external keypad : window -> bool -> err = "mlcurses_keypad"
external meta : window -> bool -> err = "mlcurses_meta"
external nodelay : window -> bool -> err = "mlcurses_nodelay"
external raw : unit -> err = "mlcurses_raw"
external noraw : unit -> err = "mlcurses_noraw"
external noqiflush : unit -> unit = "mlcurses_noqiflush"
external qiflush : unit -> unit = "mlcurses_qiflush"
external notimeout : window -> bool -> err = "mlcurses_notimeout"
external timeout : int -> unit = "mlcurses_timeout"
external wtimeout : window -> int -> unit = "mlcurses_wtimeout"
external typeahead : Unix.file_descr -> err = "mlcurses_typeahead"
(* r_err(typeahead(a_int(aa))); *)
external notypeahead : unit -> err = "mlcurses_notypeahead"
(* r_err(typeahead(-1)); *)



external def_prog_mode : unit -> unit = "mlcurses_def_prog_mode"
external def_shell_mode : unit -> unit = "mlcurses_def_shell_mode"
external reset_prog_mode : unit -> unit = "mlcurses_reset_prog_mode"
external reset_shell_mode : unit -> unit = "mlcurses_reset_shell_mode"
external resetty : unit -> unit = "mlcurses_resetty"
external savetty : unit -> unit = "mlcurses_savetty"
external getsyx : unit -> int*int = "mlcurses_getsyx"
(* int x = 0, y = 0; getsyx(y,x); r_int_int(y,x); *)
external setsyx : int -> int -> unit = "mlcurses_setsyx"
external curs_set : int -> err = "mlcurses_curs_set"
external napms : int -> unit = "mlcurses_napms"

external ripoffline : bool -> unit = "mlcurses_ripoffline"
(* ripoffline(Bool_val(aa)?1:-1,ripoff_callback); CAMLreturn(Val_unit); *)
external get_ripoff : unit -> window*int = "mlcurses_get_ripoff"
(* if(ripoff_niv==0) caml_failwith("get_ripoff"); ripoff_niv--;
  r_window_int(ripoff_w[ripoff_niv],ripoff_l[ripoff_niv]); *)





external mousemask : int -> int*int = "mlcurses_mousemask"
(* mmask_t r=1234,n=Int_val(aa); n=mousemask(n,&r); r_int_int(n,r); *)





external move : int -> int -> err = "mlcurses_move"
external wmove : window -> int -> int -> err = "mlcurses_wmove"



external clearok : window -> bool -> unit = "mlcurses_clearok"
external idlok : window -> bool -> unit = "mlcurses_idlok"
external idcok : window -> bool -> unit = "mlcurses_idcok"
external immedok : window -> bool -> unit = "mlcurses_immedok"
external leaveok : window -> bool -> unit = "mlcurses_leaveok"
external setscrreg : int -> int -> err = "mlcurses_setscrreg"
external wsetscrreg : window -> int -> int -> err = "mlcurses_wsetscrreg"
external scrollok : window -> bool -> unit = "mlcurses_scrollok"
external nl : unit -> unit = "mlcurses_nl"
external nonl : unit -> unit = "mlcurses_nonl"



external overlay : window -> window -> err = "mlcurses_overlay"
external overwrite : window -> window -> err = "mlcurses_overwrite"
external copywin : window -> window -> int -> int -> int -> int -> int -> int -> bool -> err = "mlcurses_copywin_bytecode" "mlcurses_copywin_native"



external newpad : int -> int -> window = "mlcurses_newpad"
external subpad : window -> int -> int -> int -> int -> window = "mlcurses_subpad"
external prefresh : window -> int -> int -> int -> int -> int -> int -> err = "mlcurses_prefresh_bytecode" "mlcurses_prefresh_native"
external pnoutrefresh : window -> int -> int -> int -> int -> int -> int -> err = "mlcurses_pnoutrefresh_bytecode" "mlcurses_pnoutrefresh_native"
external pechochar : window -> chtype -> err = "mlcurses_pechochar"



external refresh : unit -> err = "mlcurses_refresh"
external wrefresh : window -> err = "mlcurses_wrefresh"
external wnoutrefresh : window -> err = "mlcurses_wnoutrefresh"
external doupdate : unit -> err = "mlcurses_doupdate"
external redrawwin : window -> err = "mlcurses_redrawwin"
external wredrawln : window -> int -> int -> err = "mlcurses_wredrawln"



external wresize : window -> int -> int -> err = "mlcurses_wresize"
# 457 "_functions.c"
external resizeterm : int -> int -> err = "mlcurses_resizeterm"





external scr_dump : string -> err = "mlcurses_scr_dump"
external scr_restore : string -> err = "mlcurses_scr_restore"
external scr_init : string -> err = "mlcurses_scr_init"
external scr_set : string -> err = "mlcurses_scr_set"



external scroll : window -> err = "mlcurses_scroll"
external scrl : int -> err = "mlcurses_scrl"
external wscrl : window -> int -> err = "mlcurses_wscrl"



external slk_init : int -> err = "mlcurses_slk_init"
external slk_set : int -> string -> int -> err = "mlcurses_slk_set"
external slk_refresh : unit -> err = "mlcurses_slk_refresh"
external slk_noutrefresh : unit -> err = "mlcurses_slk_noutrefresh"
external slk_label : int -> string = "mlcurses_slk_label"
external slk_clear : unit -> err = "mlcurses_slk_clear"
external slk_restore : unit -> err = "mlcurses_slk_restore"
external slk_touch : unit -> err = "mlcurses_slk_touch"
external slk_attron : attr_t -> err = "mlcurses_slk_attron"
external slk_attroff : attr_t -> err = "mlcurses_slk_attroff"
external slk_attrset : attr_t -> err = "mlcurses_slk_attrset"



external baudrate : unit -> int = "mlcurses_baudrate"
external erasechar : unit -> char = "mlcurses_erasechar"
external has_ic : unit -> bool = "mlcurses_has_ic"
external has_il : unit -> bool = "mlcurses_has_il"
external killchar : unit -> char = "mlcurses_killchar"
external longname : unit -> string = "mlcurses_longname"
external termattrs : unit -> attr_t = "mlcurses_termattrs"
external termname : unit -> string = "mlcurses_termname"



external tgetent : string -> bool = "mlcurses_tgetent"
(* CAMLreturn(Val_bool(tgetent(NULL,String_val(aa))==1)); *)
external tgetflag : string -> bool = "mlcurses_tgetflag"
external tgetnum : string -> int = "mlcurses_tgetnum"
external tgetstr : string -> bool = "mlcurses_tgetstr"
(* r_string(tgetstr(String_val(aa),NULL)); *)
external tgoto : string -> int -> int -> string = "mlcurses_tgoto"



external setupterm : string -> Unix.file_descr -> err = "mlcurses_setupterm"
(* int r; r_err(setupterm(a_string(aa),a_int(ab),&r)); *)
external setterm : string -> err = "mlcurses_setterm"
external cur_term : unit -> terminal = "mlcurses_cur_term"
(* r_terminal(cur_term); *)
external set_curterm : terminal -> terminal = "mlcurses_set_curterm"
external del_curterm : terminal -> err = "mlcurses_del_curterm"
external restartterm : string -> Unix.file_descr -> err = "mlcurses_restartterm"
(* int r; r_err(restartterm(a_string(aa),a_int(ab),&r)); *)
external putp : string -> err = "mlcurses_putp"
external vidattr : chtype -> err = "mlcurses_vidattr"
external mvcur : int -> int -> int -> int -> err = "mlcurses_mvcur"
external tigetflag : string -> bool = "mlcurses_tigetflag"
(* r_bool(tigetflag(a_string(aa))>0); *)
external tigetnum : string -> int = "mlcurses_tigetnum"
external tigetstr : string -> string = "mlcurses_tigetstr"
(* char *s=tigetstr(a_string(aa));
  if((s==NULL)||(s==(char * )-1)) caml_failwith("tigetstr");
  CAMLreturn(caml_copy_string(s)); *)
external tputs : string -> int -> (char->unit) -> err = "mlcurses_tputs"
(* putc_function=ac;
  r_err(tputs(a_string(aa),a_int(ab),putc_callback)); *)
external vidputs : chtype -> (char->unit) -> err = "mlcurses_vidputs"
(* putc_function=ab;
  r_err(vidputs(a_chtype(aa),putc_callback)); *)
# 547 "_functions.c"
external tparm : string -> int array -> string = "mlcurses_tparm"
(* int t[10],i,n=Wosize_val(ab);
  if(n>10) n=10;
  for(i=0;i<n;i++) t[i]=a_int(Field(ab,i));
  r_string(tparm(a_string(aa),t[0],t[1],t[2],t[3],t[4],
    t[5],t[6],t[7],t[8],t[9])); *)
# 573 "_functions.c"
external bool_terminfo_variable : int -> string*string*string = "mlcurses_bool_terminfo_variable" (* CAMLlocal1(s); int n=a_int(aa); AWB(s) s=caml_alloc_tuple(3); Store_field(s,0,Val_unit); Store_field(s,1,Val_unit); Store_field(s,2,Val_unit); if((boolnames[n]==NULL)||(boolcodes[n]==NULL)||(boolfnames[n]==NULL)){ CAMLlocal1(ns); AWB(ns) ns=caml_copy_string(""); Store_field(s,0,ns); Store_field(s,1,ns); Store_field(s,2,ns); }else{ Store_field(s,0,caml_copy_string(boolnames[n])); Store_field(s,1,caml_copy_string(boolcodes[n])); Store_field(s,2,caml_copy_string(boolfnames[n])); } CAMLreturn(s); *)
external num_terminfo_variable : int -> string*string*string = "mlcurses_num_terminfo_variable" (* CAMLlocal1(s); int n=a_int(aa); AWB(s) s=caml_alloc_tuple(3); Store_field(s,0,Val_unit); Store_field(s,1,Val_unit); Store_field(s,2,Val_unit); if((numnames[n]==NULL)||(numcodes[n]==NULL)||(numfnames[n]==NULL)){ CAMLlocal1(ns); AWB(ns) ns=caml_copy_string(""); Store_field(s,0,ns); Store_field(s,1,ns); Store_field(s,2,ns); }else{ Store_field(s,0,caml_copy_string(numnames[n])); Store_field(s,1,caml_copy_string(numcodes[n])); Store_field(s,2,caml_copy_string(numfnames[n])); } CAMLreturn(s); *)
external str_terminfo_variable : int -> string*string*string = "mlcurses_str_terminfo_variable" (* CAMLlocal1(s); int n=a_int(aa); AWB(s) s=caml_alloc_tuple(3); Store_field(s,0,Val_unit); Store_field(s,1,Val_unit); Store_field(s,2,Val_unit); if((strnames[n]==NULL)||(strcodes[n]==NULL)||(strfnames[n]==NULL)){ CAMLlocal1(ns); AWB(ns) ns=caml_copy_string(""); Store_field(s,0,ns); Store_field(s,1,ns); Store_field(s,2,ns); }else{ Store_field(s,0,caml_copy_string(strnames[n])); Store_field(s,1,caml_copy_string(strcodes[n])); Store_field(s,2,caml_copy_string(strfnames[n])); } CAMLreturn(s); *)
# 584 "_functions.c"
external touchwin : window -> err = "mlcurses_touchwin"
external touchline : window -> int -> int -> err = "mlcurses_touchline"
external untouchwin : window -> err = "mlcurses_untouchwin"
external wtouchln : window -> int -> int -> bool -> err = "mlcurses_wtouchln"
external is_linetouched : window -> int -> int = "mlcurses_is_linetouched"
external is_wintouched : window -> bool = "mlcurses_is_wintouched"



external unctrl : chtype -> string = "mlcurses_unctrl"
external keyname : int -> string = "mlcurses_keyname"
external filter : unit -> unit = "mlcurses_filter"
external use_env : bool -> unit = "mlcurses_use_env"
external putwin : window -> Unix.file_descr -> err = "mlcurses_putwin"
(* int fd=dup(a_int(ab)); FILE *f=fdopen(fd,"w");
  int r=putwin(a_window(aa),f); fclose(f); r_err(r); *)
external getwin : Unix.file_descr -> window = "mlcurses_getwin"
(* int fd=dup(a_int(aa)); FILE *f=fdopen(fd,"r");
  int r=putwin(a_window(aa),f); fclose(f); r_err(r); *)
external delay_output : int -> err = "mlcurses_delay_output"
external flushinp : unit -> unit = "mlcurses_flushinp"



external newwin : int -> int -> int -> int -> window = "mlcurses_newwin"
external delwin : window -> err = "mlcurses_delwin"
external mvwin : window -> int -> int -> err = "mlcurses_mvwin"
external subwin : window -> int -> int -> int -> int -> window = "mlcurses_subwin"
external derwin : window -> int -> int -> int -> int -> window = "mlcurses_derwin"
external mvderwin : window -> int -> int -> err = "mlcurses_mvderwin"
external dupwin : window -> window = "mlcurses_dupwin"
external wsyncup : window -> unit = "mlcurses_wsyncup"
external syncok : window -> bool -> err = "mlcurses_syncok"
external wcursyncup : window -> unit = "mlcurses_wcursyncup"
external wsyncdown : window -> unit = "mlcurses_wsyncdown"




external get_acs_codes : unit -> Acs.acs = "mlcurses_get_acs_codes"
(* CAMLlocal1(tr); AWB(tr)
  tr=caml_alloc_tuple(32);
Store_field(tr,0,Val_int(ACS_ULCORNER)); Store_field(tr,1,Val_int(ACS_LLCORNER)); Store_field(tr,2,Val_int(ACS_URCORNER)); Store_field(tr,3,Val_int(ACS_LRCORNER));
Store_field(tr,4,Val_int(ACS_LTEE)); Store_field(tr,5,Val_int(ACS_RTEE)); Store_field(tr,6,Val_int(ACS_BTEE)); Store_field(tr,7,Val_int(ACS_TTEE));
Store_field(tr,8,Val_int(ACS_HLINE)); Store_field(tr,9,Val_int(ACS_VLINE)); Store_field(tr,10,Val_int(ACS_PLUS)); Store_field(tr,11,Val_int(ACS_S1));
Store_field(tr,12,Val_int(ACS_S9)); Store_field(tr,13,Val_int(ACS_DIAMOND)); Store_field(tr,14,Val_int(ACS_CKBOARD)); Store_field(tr,15,Val_int(ACS_DEGREE));
Store_field(tr,16,Val_int(ACS_PLMINUS)); Store_field(tr,17,Val_int(ACS_BULLET)); Store_field(tr,18,Val_int(ACS_LARROW)); Store_field(tr,19,Val_int(ACS_RARROW));
Store_field(tr,20,Val_int(ACS_DARROW)); Store_field(tr,21,Val_int(ACS_UARROW)); Store_field(tr,22,Val_int(ACS_BOARD)); Store_field(tr,23,Val_int(ACS_LANTERN));
Store_field(tr,24,Val_int(ACS_BLOCK)); Store_field(tr,25,Val_int(ACS_S3)); Store_field(tr,26,Val_int(ACS_S7)); Store_field(tr,27,Val_int(ACS_LEQUAL));
Store_field(tr,28,Val_int(ACS_GEQUAL)); Store_field(tr,29,Val_int(ACS_PI)); Store_field(tr,30,Val_int(ACS_NEQUAL)); Store_field(tr,31,Val_int(ACS_STERLING));
  CAMLreturn(tr);
*)




external winch_handler_on : unit -> unit = "mlcurses_winch_handler_on"
(* signal(SIGWINCH,winch_handler); CAMLreturn(Val_unit); *)
external winch_handler_off : unit -> unit = "mlcurses_winch_handler_off"
(* signal(SIGWINCH,SIG_IGN); CAMLreturn(Val_unit); *)





external get_size : unit -> int*int = "mlcurses_get_size"
(*

  struct winsize ws;
  ioctl(0,TIOCGWINSZ,&ws);
  r_int_int(ws.ws_row,ws.ws_col);







*)
external get_size_fd : Unix.file_descr -> int*int = "mlcurses_get_size_fd"
(*

  struct winsize ws;
  ioctl(a_int(aa),TIOCGWINSZ,&ws);
  r_int_int(ws.ws_row,ws.ws_col);







*)
# 112 "_curses.ml" 2

(* these two were written separately in ml_curses.c,
 * to permit proper threading behavior *)
external getch : unit -> int = "mlcurses_getch"
external wgetch : window -> int = "mlcurses_wgetch"
external getnstr_inner : int -> (string * err) = "mlcurses_getnstr_inner"

(* Wrap the inner function so we can return a result *)
let tuple_to_result (str, err) =
  match err with
  | false -> Error ()
  | true -> Ok str
;;

let getnstr n = tuple_to_result (getnstr_inner n)
let getstr () = getnstr 1024

let null_window = null_window ()

let bool_terminfo_variables = Hashtbl.create 67
let num_terminfo_variables = Hashtbl.create 67
let str_terminfo_variables = Hashtbl.create 601

let () =
  let rec ins f h n =
    let (a, b, c) = f n in
    if a = "" then ()
    else (
      Hashtbl.add h c (a, b);
      ins f h (n + 1)
    ) in
  (* These functions do not exist on all curses implementations,
   * so if they throw Invalid_argument, just ignore it.
   *)
  try
    ins bool_terminfo_variable bool_terminfo_variables 0;
    ins num_terminfo_variable num_terminfo_variables 0;
    ins str_terminfo_variable str_terminfo_variables 0
  with
    Invalid_argument _ -> ()
# 160 "_curses.ml"
module A = struct
  let normal = 0
  let attributes = 0x7FFFFF00
  let chartext = 0x000000FF
  let color = 0x0000FF00
  let standout = 0x00010000
  let underline = 0x00020000
  let reverse = 0x00040000
  let blink = 0x00080000
  let dim = 0x00100000
  let bold = 0x00200000
  let altcharset = 0x00400000
  let invis = 0x00800000
  let protect = 0x01000000
  let horizontal = 0x02000000
  let left = 0x04000000
  let low = 0x08000000
  let right = 0x10000000
  let top = 0x20000000
  let vertical = 0x40000000
  let combine = List.fold_left (lor) 0
  let color_pair n = (n lsl 8) land color
  let pair_number a = (a land color) lsr 8
end

(* *)
module WA = A

module Color = struct
  let black = 0
  let red = 1
  let green = 2
  let yellow = 3
  let blue = 4
  let magenta = 5
  let cyan = 6
  let white = 7
end

module Key = struct
# 1 "_keys.ml" 1
let code_yes = 0o400
let min = 0o401
let break = 0o401
let down = 0o402
let up = 0o403
let left = 0o404
let right = 0o405
let home = 0o406
let backspace = 0o407
let f0 = 0o410
let dl = 0o510
let il = 0o511
let dc = 0o512
let ic = 0o513
let eic = 0o514
let clear = 0o515
let eos = 0o516
let eol = 0o517
let sf = 0o520
let sr = 0o521
let npage = 0o522
let ppage = 0o523
let stab = 0o524
let ctab = 0o525
let catab = 0o526
let enter = 0o527
let sreset = 0o530
let reset = 0o531
let print = 0o532
let ll = 0o533
let a1 = 0o534
let a3 = 0o535
let b2 = 0o536
let c1 = 0o537
let c3 = 0o540
let btab = 0o541
let beg = 0o542
let cancel = 0o543
let close = 0o544
let command = 0o545
let copy = 0o546
let create = 0o547
let end_ = 0o550
let exit = 0o551
let find = 0o552
let help = 0o553
let mark = 0o554
let message = 0o555
let move = 0o556
let next = 0o557
let open_ = 0o560
let options = 0o561
let previous = 0o562
let redo = 0o563
let reference = 0o564
let refresh = 0o565
let replace = 0o566
let restart = 0o567
let resume = 0o570
let save = 0o571
let sbeg = 0o572
let scancel = 0o573
let scommand = 0o574
let scopy = 0o575
let screate = 0o576
let sdc = 0o577
let sdl = 0o600
let select = 0o601
let send = 0o602
let seol = 0o603
let sexit = 0o604
let sfind = 0o605
let shelp = 0o606
let shome = 0o607
let sic = 0o610
let sleft = 0o611
let smessage = 0o612
let smove = 0o613
let snext = 0o614
let soptions = 0o615
let sprevious = 0o616
let sprint = 0o617
let sredo = 0o620
let sreplace = 0o621
let sright = 0o622
let srsume = 0o623
let ssave = 0o624
let ssuspend = 0o625
let sundo = 0o626
let suspend = 0o627
let undo = 0o630
let mouse = 0o631
let resize = 0o632
let max = 0o777
# 201 "_curses.ml" 2
  let f n = f0 + n
end

module Curses_config = struct
# 1 "_config.ml" 1
let wide_ncurses = true
# 206 "_curses.ml" 2
end

let lC_CTYPE = 0
let lC_NUMERIC = 1
let lC_TIME = 2
let lC_COLLATE = 3
let lC_MONETARY = 4
let lC_MESSAGES = 5
let lC_ALL = 6

external setlocale : int -> string -> string option = "mlcurses_setlocale"
