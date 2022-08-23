class ZCL_ZTEST99_HTTP definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZTEST99_HTTP IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.

    data lv_html type string.

    lv_html = |<html>\n| &
              |<head>\n| &
              |    <meta http-equiv="X-UA-Compatible" content="IE=edge"/>\n| &
              |    <meta charset="UTF-8">\n| &
              | \n| &
              |    <title>UI5 One Pager</title>\n| &
              | \n| &
              |    <script id="sap-ui-bootstrap"\n| &
              |            src="https://openui5.hana.ondemand.com/resources/sap-ui-core.js"\n| &
              |            data-sap-ui-libs="sap.m"\n| &
              |            data-sap-ui-theme="sap_bluecrystal"\n| &
              |            data-sap-ui-preload="async"\n| &
              |            data-sap-ui-compatVersion="edge"\n| &
              |            data-sap-ui-frameOptions='allow'>    // NON-SECURE setting for test environment\n| &
              |    </script>\n| &
              | \n| &
              |    <script id="view1" type="ui5/xmlview">\n| &
              |        <mvc:View\n| &
              |            controllerName="controller1"\n| &
              |            displayBlock="true"\n| &
              |            xmlns="sap.m"\n| &
              |            xmlns:l="sap.ui.layout"\n| &
              |            xmlns:core="sap.ui.core"\n| &
              |            xmlns:mvc="sap.ui.core.mvc">\n| &
              |            <Page title="$\{NAME\}">\n| &
              |                <content>\n| &
              |                    <Button press="pressBtn" text="Press me"/>\n| &
              |                </content>\n| &
              |            </Page>\n| &
              |        </mvc:View>\n| &
              |    </script>\n| &
              |    <script>\n| &
              |        sap.ui.getCore().attachInit(function () \{\n| &
              |            "use strict";\n| &
              | \n| &
              |            sap.ui.define([\n| &
              |                "sap/ui/core/mvc/Controller"\n| &
              |            ], function (Controller) \{\n| &
              |                return Controller.extend("controller1", \{\n| &
              |                    pressBtn: function (oEvent) \{\n| &
              |                        sap.m.MessageToast.show("Hello UI5 DevLife!");\n| &
              |                    \}\n| &
              |                \});\n| &
              |            \});\n| &
              | \n| &
              |            sap.ui.require([\n| &
              |                "jquery.sap.global"\n| &
              |            ], function (jQuery) \{\n| &
              |                sap.ui.xmlview(\{\n| &
              |                    viewContent: jQuery("#view1").html()\n| &
              |                \}).placeAt('root');\n| &
              |            \});\n| &
              | \n| &
              |        \});\n| &
              |    </script>\n| &
              |</head>\n| &
              |<body class="sapUiBody" id="root"/>\n| &
              |</html>|.

  response->set_text( lv_html ).

  endmethod.
ENDCLASS.
