-module(mf_id).

id_msg_convert(pb1) -> 1;
id_msg_convert(1) -> pb1;
id_msg_convert(pb1_s2c_addressbook) -> 258;
id_msg_convert(258) -> pb1_s2c_addressbook;
id_msg_convert(pb1_c2s_addressbook) -> 257;
id_msg_convert(257) -> pb1_c2s_addressbook;

id_msg_convert(pb2) -> 2;
id_msg_convert(2) -> pb2;
id_msg_convert(pb2_s2c_nimei) -> 514;
id_msg_convert(514) -> pb2_s2c_nimei;
id_msg_convert(pb2_c2s_nimei) -> 513;
id_msg_convert(513) -> pb2_c2s_nimei;

id_msg_convert(_Arg) -> io:format("invalid arg:~p", [_Arg]), error(invalid_is_msg_convert).