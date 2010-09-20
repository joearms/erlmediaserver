{application, erlmediaserver,
 [{description, "a media server"},
 {vsn, "1.0"},
 {modules, [erlmediaserver, mediaserver, root_device, handler, sax,
 			radiocontentdirectory, radiocontentdirectory_handler,
 			radioconnectionmanager, description_handler,
 			contentdirectory, contentdirectory_handler,
 			connectionmanager_handler, connectionmanager, 
 			ssdp, ssdp_msg_factory,
 			cds_lib, cds_to_xml, os_info, stringplus,
 			uuid, xml_to_cds, xmltool
 			]},
 {registered, [erlmediaserver]},
 {mod, {erlmediaserver,[]}},
 {applications, [kernel, stdlib]},
 {env, [
 	{port, 5001},
 	{ip, {0,0,0,0}},
 	{working_dir, "/Users/ua/projekte/erlang/lilly/erlmediaserver"},
    {timer, 1},
    {services, [root_device, mediaserver, contentdirectory, connectionmanager, radiocontentdirectory, radioconnectionmanager]},
    {upnp, "UPnP/1.0 ERL_PMS/1.0"},
    {dirs, ["/Users/ulfangermann/projects/erlang/lilly/erlmediaserver/radiodb"]}
    ]}
 ]}.
