## base16-kakoune (https://github.com/leira/base16-kakoune)
## by Leira Hua
## Dracula scheme by Mike Barkmin (http://github.com/mikebarkmin) based on Dracula Theme (http://github.com/dracula)

try %{
    decl str base00
    decl str base01
    decl str base02
    decl str base03
    decl str base04
    decl str base05
    decl str base06
    decl str base07
    decl str base08
    decl str base09
    decl str base0A
    decl str base0B
    decl str base0C
    decl str base0D
    decl str base0E
    decl str base0F
}

set global base00 rgb:282936
set global base01 rgb:3a3c4e
set global base02 rgb:4d4f68
set global base03 rgb:626483
set global base04 rgb:62d6e8
set global base05 rgb:e9e9f4
set global base06 rgb:f1f2f8
set global base07 rgb:f7f7fb
set global base08 rgb:ea51b2
set global base09 rgb:b45bcf
set global base0A rgb:00f769
set global base0B rgb:ebff87
set global base0C rgb:a1efe4
set global base0D rgb:62d6e8
set global base0E rgb:b45bcf
set global base0F rgb:00f769

## code
face global value              "%opt{base09},%opt{base00}"
face global type               "%opt{base0A},%opt{base00}"
face global identifier         "%opt{base08},%opt{base00}"
face global string             "%opt{base0B},%opt{base00}"
face global keyword            "%opt{base0E},%opt{base00}"
face global operator           "%opt{base05},%opt{base00}"
face global attribute          "%opt{base0C},%opt{base00}"
face global comment            "%opt{base0C},%opt{base00}"
face global meta               "%opt{base0D},%opt{base00}"
face global builtin            "%opt{base0D},%opt{base00}+b"

## markup
face global title              "%opt{base0D},%opt{base00}+b"
face global header             "%opt{base0D},%opt{base00}+b"
face global bold               "%opt{base0A},%opt{base00}+b"
face global italic             "%opt{base0E},%opt{base00}"
face global mono               "%opt{base0B},%opt{base00}"
face global block              "%opt{base0C},%opt{base00}"
face global link               "%opt{base09},%opt{base00}"
face global bullet             "%opt{base08},%opt{base00}"
face global list               "%opt{base08},%opt{base00}"

## builtin
face global Default            "%opt{base05},%opt{base00}"
face global PrimaryCursor      "%opt{base00},%opt{base0E}"
face global PrimarySelection   "%opt{base00},%opt{base0C}"
face global SecondaryCursor    "%opt{base00},%opt{base05}"
face global SecondarySelection "%opt{base00},%opt{base04}"
face global LineNumbers        "%opt{base02},%opt{base00}"
face global LineNumberCursor   "%opt{base0A},%opt{base00}"
face global MenuForeground     "%opt{base00},%opt{base0D}"
face global MenuBackground     "%opt{base00},%opt{base0C}"
face global MenuInfo           "%opt{base02},%opt{base0C}"
face global Information        "%opt{base00},%opt{base0A}"
face global Error              "%opt{base00},%opt{base08}"
face global StatusLine         "%opt{base04},%opt{base01}"
face global StatusLineMode     "%opt{base0B},%opt{base01}"
face global StatusLineInfo     "%opt{base0D},%opt{base01}"
face global StatusLineValue    "%opt{base0C},%opt{base01}"
face global StatusCursor       "%opt{base00},%opt{base05}"
face global Prompt             "%opt{base0D},%opt{base01}"
face global MatchingChar       "%opt{base06},%opt{base02}+b"
face global BufferPadding      "%opt{base03},%opt{base00}"
