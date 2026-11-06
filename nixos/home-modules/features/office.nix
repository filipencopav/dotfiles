{ ... }:
let
  onlyoffice = "onlyoffice-desktopeditors.desktop";
in
{
  programs.onlyoffice = {
    enable = true;
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      # Word documents
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = [ onlyoffice ]; # .docx
      "application/msword" = [ onlyoffice ]; # .doc
      "application/vnd.openxmlformats-officedocument.wordprocessingml.template" = [ onlyoffice ]; # .dotx
      "application/rtf" = [ onlyoffice ];
      "text/rtf" = [ onlyoffice ];

      # Spreadsheets
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = [ onlyoffice ]; # .xlsx
      "application/vnd.ms-excel" = [ onlyoffice ]; # .xls
      "application/vnd.openxmlformats-officedocument.spreadsheetml.template" = [ onlyoffice ]; # .xltx
      "text/csv" = [ onlyoffice ];

      # Presentations
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" = [ onlyoffice ]; # .pptx
      "application/vnd.ms-powerpoint" = [ onlyoffice ]; # .ppt
      "application/vnd.openxmlformats-officedocument.presentationml.slideshow" = [ onlyoffice ]; # .ppsx

      # OpenDocument formats (optional — include if you want OnlyOffice over LO for these too)
      "application/vnd.oasis.opendocument.text" = [ onlyoffice ]; # .odt
      "application/vnd.oasis.opendocument.spreadsheet" = [ onlyoffice ]; # .ods
      "application/vnd.oasis.opendocument.presentation" = [ onlyoffice ]; # .odp
    };
  };

}
