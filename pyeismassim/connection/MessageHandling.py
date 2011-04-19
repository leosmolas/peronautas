# -*- coding: utf-8 -*-

from BeautifulSoup import BeautifulStoneSoup
# Author: Iñaki Garay
#
# Functions:
#   parse(xml_string)
#       parses a string representing an xml message from the server
#   print_message(parse_message)
#       pretty prints a parsed xml message
#   auth_request(username, password)
#       generates a string representing an autorization request xml message
#   bye()
#       generates a string representing a goodbye xml message
#   action(action_id, action_type[, action_parameter])
#       generates a string representing an action xml message

# Parsing

def parse_auth_response(xml):
    message_tag        = xml.contents[1]
    authentication_tag = message_tag.contents[0]

    timestamp_attr = message_tag['timestamp']
    type_attr      = message_tag['type']
    result_attr    = authentication_tag['result']

    result              = {}
    result['timestamp'] = timestamp_attr
    result['type']      = type_attr
    result['result']    = result_attr

    return result

def parse_sim_start(xml):
    message_tag    = xml.contents[1]
    simulation_tag = message_tag.contents[0]

    timestamp_attr = message_tag['timestamp']
    type_attr      = message_tag['type']
    edges_attr     = simulation_tag['edges'] 
    id_attr        = simulation_tag['id']
    steps_attr     = simulation_tag['steps']
    vertices_attr  = simulation_tag['vertices']

    result              = {}
    result['timestamp'] = timestamp_attr
    result['type']      = type_attr
    result['edges']     = edges_attr
    result['id']        = id_attr
    result['steps']     = steps_attr
    result['vertices']  = vertices_attr

    return result

def parse_sim_end(xml):
    message_tag    = xml.contents[1]
    sim_result_tag = message_tag.contents[0]

    timestamp_attr = message_tag['timestamp']
    type_attr      = message_tag['type']
    ranking_attr   = sim_result_tag['ranking']
    score_attr     = sim_result_tag['score']

    result              = {}
    result['timestamp'] = timestamp_attr
    result['type']      = type_attr
    result['ranking']   = ranking_attr
    result['score']     = score_attr

    return result

def parse_request_action(xml):
    message_tag        = xml.find('message')
    perception_tag     = xml.find('perception')
    simulation_tag     = xml.find('simulation')
    self_tag           = xml.find('self')
    team_tag           = xml.find('team')
    vis_verts_tag      = xml.find('visiblevertices')
    vis_edges_tag      = xml.find('visibleedges')
    vis_ents_tag       = xml.find('visibleentities')
    probed_verts_tag   = xml.find('probedvertices')
    surveyed_edges_tag = xml.find('surveyededges')
    inspected_ents_tag = xml.find('inspectedentities')
    achievements_tag   = xml.find('achievements')

    result                        = {}
    result['timestamp']           = message_tag['timestamp']      
    result['type']                = message_tag['type']           
    result['deadline']            = perception_tag['deadline']    
    result['id']                  = perception_tag['id']          
    result['step']                = simulation_tag['step']        
    result['energy']              = self_tag['energy']            
    result['health']              = self_tag['health']            
    result['last_action']         = self_tag['lastaction']        
    result['last_action_result']  = self_tag['lastactionresult']  
    result['max_energy']          = self_tag['maxenergy']         
    result['max_energy_disabled'] = self_tag['maxenergydisabled'] 
    result['max_health']          = self_tag['maxhealth']         
    result['position']            = self_tag['position']          
    result['strength']            = self_tag['strength']          
    result['vis_range']           = self_tag['visrange']          
    result['zone_score']          = self_tag['zonescore']         
    result['last_step_score']     = team_tag['laststepscore']     
    result['money']               = team_tag['money']             
    result['score']               = team_tag['score']             

    if (achievements_tag != None):
        achievements_list = []
        for i in range(len(achievements_tag.contents)):
            achievements_list.append(achievements_tag.contents[i]['name'])
        result['achievements'] = achievements_list
    
    if (vis_verts_tag != None):
        # This check is done because if the xml has no visible vertices tag, it will be None.
        vis_verts_list = []
        for i in range(len(vis_verts_tag.contents)):
            # This check is done because the contents of the tag will be a list of objects which may be of class Tag or class NavigableString.
            # In the latter case, it is most probably a space or junk and will not be indexable with strings.
            if (vis_verts_tag.contents[i].__class__.__name__ == "Tag"):
                vis_vert = {}
                vis_vert['name'] = vis_verts_tag.contents[i]['name']
                vis_vert['team'] = vis_verts_tag.contents[i]['team']
                vis_verts_list.append(vis_vert)
        result['vis_verts'] = vis_verts_list

    if (vis_edges_tag != None):
        vis_edges_list = []
        for i in range(len(vis_edges_tag.contents)):
            if (vis_edges_tag.contents[i].__class__.__name__ == "Tag"):
                vis_edge = {}
                vis_edge['node1'] = vis_edges_tag.contents[i]['node1']
                vis_edge['node2'] = vis_edges_tag.contents[i]['node2']
                vis_edges_list.append(vis_edge)
            result['vis_edges'] = vis_edges_list

    if (vis_ents_tag != None):
        vis_ents_list = []
        for i in range(len(vis_ents_tag.contents)):
            if (vis_ents_tag.contents[i].__class__.__name__ == "Tag"):
                vis_ent = {}
                vis_ent['name'] = vis_ents_tag.contents[i]['name']
                vis_ent['node'] = vis_ents_tag.contents[i]['node']
                vis_ent['team'] = vis_ents_tag.contents[i]['team']
                vis_ents_list.append(vis_ent)
        result['vis_ents'] = vis_ents_list

    if (probed_verts_tag != None):
        probed_verts_list = []
        for i in range(len(probed_verts_tag.contents)):
            if (probed_verts_tag.contents[i].__class__.__name__ == "Tag"):
                probed_vert = {}
                probed_vert['name']  = probed_verts_tag.contents[i]['name']
                probed_vert['value'] = probed_verts_tag.contents[i]['value']
                probed_verts_list.append(probed_vert)
        result['probed_verts'] = probed_verts_list

    if (surveyed_edges_tag != None):
        surveyed_edges_list = []
        for i in range(len(surveyed_edges_tag.contents)):
            if (surveyed_edges_tag.contents[i].__class__.__name__ == "Tag"):
                surveyed_edge = {}
                surveyed_edge['node1']  = surveyed_edges_tag.contents[i]['node1']
                surveyed_edge['node2']  = surveyed_edges_tag.contents[i]['node2']
                surveyed_edge['weight'] = surveyed_edges_tag.contents[i]['weight']
        result['survey_edges'] = surveyed_edges_list

    if (inspected_ents_tag != None):
        inspected_ents_list = []
        for i in range(len(inspected_ents_tag.contents)):
            if (inspected_ents_tag.contents[i].__class__.__name__ == "Tag"):
                inspected_ent = {}
                inspected_ent['energy']     = inspected_ents_tag.contents[i]['energy']
                inspected_ent['health']     = inspected_ents_tag.contents[i]['health']
                inspected_ent['max_energy'] = inspected_ents_tag.contents[i]['max_energy']
                inspected_ent['max_health'] = inspected_ents_tag.contents[i]['max_health']
                inspected_ent['name']       = inspected_ents_tag.contents[i]['name']
                inspected_ent['node']       = inspected_ents_tag.contents[i]['node']
                inspected_ent['role']       = inspected_ents_tag.contents[i]['role']
                inspected_ent['strength']   = inspected_ents_tag.contents[i]['strength']
                inspected_ent['team']       = inspected_ents_tag.contents[i]['team']
                inspected_ent['vis_range']  = inspected_ents_tag.contents[i]['vis_range']
                inspected_ents_list.append(inspected_ent)
        result['inspected_ents'] = inspected_ents_list

    return result

def parse(msg):
    parse_functions = { "auth-response"  : parse_auth_response,
                        "sim-start"      : parse_sim_start,
                        "sim-end"        : parse_sim_end,
                        "request-action" : parse_request_action 
                      }
    xml          = BeautifulStoneSoup(msg, selfClosingTags=['authentication'])
    message_type = xml.contents[1]['type']
    return parse_functions[message_type](xml)

def print_message(result):
    """
    Use this function to pretty-print a parsed message.
    """
    for key, value in result.iteritems():
        if (value.__class__.__name__ == 'list'):
            print "   ", key
            for i in value:
                print "       ",
                for k2, v2  in i.iteritems():
                    print k2, ":", v2, "|", 
                print
        else:
            print "   ", key, ":", value

# Generation

def auth_request(username, password):
    return '<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="auth-request"><authentication password="%s" username="%s"/></message>' % (password, username)

def bye():
    return '<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="bye"></message>'

def action(action_id, action_type, action_parameter = None):
    """
    This function creates string representing the xml encoding of an action. 
    'action_id' must be the action identifier obtained from the received request-action xml message.
    'action_type' may be one of: 'parry', 'probe', 'survey', 'inspect', 'recharge', 'skip', 'goto', 'attack', 'repair', 'buy'

    If the action requires a parameter, pass it as the third argument. 
    If the action type is not one of those listed, the message will not be valid but will still be generated.
    If the parameter is not what the MASSim server expects, the message will not be valid but will still be generated.
    """
    if (action_parameter == None):
        return u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="action"><action id="%s" type="%s"/></message>' % (action_id, action_type)
    else:
        return u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="action"><action id="%s" type="%s" param="%s"/></message>' % (action_id, action_type, action_parameter)

if (__name__ == "__main__"):
    print auth_request("USER", "PASS")
    print bye()
    print action("action-id", "parry")
    print action("action-id", "probe")
    print action("action-id", "survey")
    print action("action-id", "inspect")
    print action("action-id", "recharge")
    print action("action-id", "skip")
    print action("action-id", "goto", "vertex1")
    print action("action-id", "attack", "agent1")
    print action("action-id", "repair", "agent1")
    print action("action-id", "buy", "battery")

    auth_response   = '<?xml version="1.0" encoding="UTF-8" standalone="no"?><message timestamp="1297263037617" type="auth-response"><authentication result="ok"></message>'
    sim_start       = '<?xml version="1.0" encoding="UTF-8"?><message timestamp="1302894016107" type="sim-start"><simulation edges="44" id="0" steps="1000" vertices="20"/></message>'
    request_action1 = '<?xml version="1.0" encoding="UTF-8"?><message timestamp="1302894016202" type="request-action"><perception deadline="1302894018202" id="1"><simulation step="0"/><self energy="12" health="4" lastAction="skip" lastActionResult="successful" maxEnergy="12" maxEnergyDisabled="12" maxHealth="4" position="vertex1" strength="0" visRange="2" zoneScore="0"/><team lastStepScore="0" money="0" score="0" zonesScore="0"/><visibleVertices><visibleVertex name="vertex4" team="none"/><visibleVertex name="vertex12" team="none"/><visibleVertex name="vertex19" team="none"/> <visibleVertex name="vertex15" team="none"/><visibleVertex name="vertex11" team="none"/><visibleVertex name="vertex17" team="none"/> <visibleVertex name="vertex7" team="none"/> <visibleVertex name="vertex1" team="none"/> <visibleVertex name="vertex2" team="none"/> <visibleVertex name="vertex3" team="none"/> <visibleVertex name="vertex9" team="none"/> <visibleVertex name="vertex16" team="none"/> </visibleVertices> <visibleEdges> <visibleEdge node1="vertex1" node2="vertex7"/> <visibleEdge node1="vertex3" node2="vertex15"/> <visibleEdge node1="vertex15" node2="vertex19"/> <visibleEdge node1="vertex9" node2="vertex16"/> <visibleEdge node1="vertex4" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex3"/> <visibleEdge node1="vertex9" node2="vertex11"/> <visibleEdge node1="vertex1" node2="vertex15"/> <visibleEdge node1="vertex9" node2="vertex12"/> <visibleEdge node1="vertex7" node2="vertex17"/> <visibleEdge node1="vertex2" node2="vertex7"/> <visibleEdge node1="vertex1" node2="vertex2"/> <visibleEdge node1="vertex9" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex9"/> <visibleEdge node1="vertex7" node2="vertex12"/> </visibleEdges> <visibleEntities> <visibleEntity name="a9" node="vertex4" status="normal" team="A"/> <visibleEntity name="b2" node="vertex1" status="normal" team="B"/> <visibleEntity name="a5" node="vertex7" status="normal" team="A"/> <visibleEntity name="b7" node="vertex2" status="normal" team="B"/> <visibleEntity name="a6" node="vertex2" status="normal" team="A"/> <visibleEntity name="a2" node="vertex4" status="normal" team="A"/> <visibleEntity name="a10" node="vertex12" status="normal" team="A"/> <visibleEntity name="b5" node="vertex2" status="normal" team="B"/> <visibleEntity name="a3" node="vertex12" status="normal" team="A"/> <visibleEntity name="a1" node="vertex1" status="normal" team="A"/> <visibleEntity name="b4" node="vertex4" status="normal" team="B"/> <visibleEntity name="b8" node="vertex3" status="normal" team="B"/> </visibleEntities> </perception> </message>'
    request_action2 = '<?xml version="1.0" encoding="UTF-8"?><message timestamp="1302894018781" type="request-action"> <perception deadline="1302894020781" id="2"> <simulation step="1"/> <self energy="12" health="4" lastAction="skip" lastActionResult="successful" maxEnergy="12" maxEnergyDisabled="12" maxHealth="4" position="vertex1" strength="0" visRange="2" zoneScore="0"/> <team lastStepScore="8" money="0" score="8" zonesScore="8"/> <visibleVertices> <visibleVertex name="vertex4" team="A"/> <visibleVertex name="vertex12" team="A"/> <visibleVertex name="vertex19" team="none"/> <visibleVertex name="vertex15" team="none"/> <visibleVertex name="vertex11" team="A"/> <visibleVertex name="vertex17" team="A"/> <visibleVertex name="vertex7" team="A"/> <visibleVertex name="vertex1" team="none"/> <visibleVertex name="vertex2" team="B"/> <visibleVertex name="vertex3" team="B"/> <visibleVertex name="vertex9" team="A"/> <visibleVertex name="vertex16" team="none"/> </visibleVertices> <visibleEdges> <visibleEdge node1="vertex1" node2="vertex7"/> <visibleEdge node1="vertex3" node2="vertex15"/> <visibleEdge node1="vertex15" node2="vertex19"/> <visibleEdge node1="vertex9" node2="vertex16"/> <visibleEdge node1="vertex4" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex3"/> <visibleEdge node1="vertex9" node2="vertex11"/> <visibleEdge node1="vertex1" node2="vertex15"/> <visibleEdge node1="vertex9" node2="vertex12"/> <visibleEdge node1="vertex7" node2="vertex17"/> <visibleEdge node1="vertex2" node2="vertex7"/> <visibleEdge node1="vertex1" node2="vertex2"/> <visibleEdge node1="vertex9" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex9"/> <visibleEdge node1="vertex7" node2="vertex12"/> </visibleEdges> <visibleEntities> <visibleEntity name="a9" node="vertex4" status="normal" team="A"/> <visibleEntity name="b2" node="vertex1" status="normal" team="B"/> <visibleEntity name="a5" node="vertex7" status="normal" team="A"/> <visibleEntity name="b7" node="vertex2" status="normal" team="B"/> <visibleEntity name="a6" node="vertex2" status="normal" team="A"/> <visibleEntity name="a2" node="vertex4" status="normal" team="A"/> <visibleEntity name="a10" node="vertex12" status="normal" team="A"/> <visibleEntity name="b5" node="vertex2" status="normal" team="B"/> <visibleEntity name="a3" node="vertex12" status="normal" team="A"/> <visibleEntity name="a1" node="vertex1" status="normal" team="A"/> <visibleEntity name="b4" node="vertex4" status="normal" team="B"/> <visibleEntity name="b8" node="vertex3" status="normal" team="B"/> </visibleEntities> </perception> </message>'
    request_action3 = '<?xml version="1.0" encoding="UTF-8"?><message timestamp="1302894021363" type="request-action"> <perception deadline="1302894023363" id="3"> <simulation step="2"/> <self energy="12" health="4" lastAction="skip" lastActionResult="successful" maxEnergy="12" maxEnergyDisabled="12" maxHealth="4" position="vertex1" strength="0" visRange="2" zoneScore="0"/> <team lastStepScore="8" money="0" score="16" zonesScore="8"/> <visibleVertices> <visibleVertex name="vertex4" team="A"/> <visibleVertex name="vertex12" team="A"/> <visibleVertex name="vertex19" team="none"/> <visibleVertex name="vertex15" team="none"/> <visibleVertex name="vertex11" team="A"/> <visibleVertex name="vertex17" team="A"/> <visibleVertex name="vertex7" team="A"/> <visibleVertex name="vertex1" team="none"/> <visibleVertex name="vertex2" team="B"/> <visibleVertex name="vertex3" team="B"/> <visibleVertex name="vertex9" team="A"/> <visibleVertex name="vertex16" team="none"/> </visibleVertices> <visibleEdges> <visibleEdge node1="vertex1" node2="vertex7"/> <visibleEdge node1="vertex3" node2="vertex15"/> <visibleEdge node1="vertex15" node2="vertex19"/> <visibleEdge node1="vertex9" node2="vertex16"/> <visibleEdge node1="vertex4" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex3"/> <visibleEdge node1="vertex9" node2="vertex11"/> <visibleEdge node1="vertex1" node2="vertex15"/> <visibleEdge node1="vertex9" node2="vertex12"/> <visibleEdge node1="vertex7" node2="vertex17"/> <visibleEdge node1="vertex2" node2="vertex7"/> <visibleEdge node1="vertex1" node2="vertex2"/> <visibleEdge node1="vertex9" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex9"/> <visibleEdge node1="vertex7" node2="vertex12"/> </visibleEdges> <visibleEntities> <visibleEntity name="a9" node="vertex4" status="normal" team="A"/> <visibleEntity name="b2" node="vertex1" status="normal" team="B"/> <visibleEntity name="a5" node="vertex7" status="normal" team="A"/> <visibleEntity name="b7" node="vertex2" status="normal" team="B"/> <visibleEntity name="a6" node="vertex2" status="normal" team="A"/> <visibleEntity name="a2" node="vertex4" status="normal" team="A"/> <visibleEntity name="a10" node="vertex12" status="normal" team="A"/> <visibleEntity name="b5" node="vertex2" status="normal" team="B"/> <visibleEntity name="a3" node="vertex12" status="normal" team="A"/> <visibleEntity name="a1" node="vertex1" status="normal" team="A"/> <visibleEntity name="b4" node="vertex4" status="normal" team="B"/> <visibleEntity name="b8" node="vertex3" status="normal" team="B"/> </visibleEntities> </perception> </message>'
    request_action4 = '<?xml version="1.0" encoding="UTF-8"?><message timestamp="1302894023957" type="request-action"> <perception deadline="1302894025957" id="4"> <simulation step="3"/> <self energy="12" health="4" lastAction="skip" lastActionResult="successful" maxEnergy="12" maxEnergyDisabled="12" maxHealth="4" position="vertex1" strength="0" visRange="2" zoneScore="0"/> <team lastStepScore="8" money="0" score="24" zonesScore="8"/> <visibleVertices> <visibleVertex name="vertex4" team="A"/> <visibleVertex name="vertex12" team="A"/> <visibleVertex name="vertex19" team="none"/> <visibleVertex name="vertex15" team="none"/> <visibleVertex name="vertex11" team="A"/> <visibleVertex name="vertex17" team="A"/> <visibleVertex name="vertex7" team="A"/> <visibleVertex name="vertex1" team="none"/> <visibleVertex name="vertex2" team="B"/> <visibleVertex name="vertex3" team="B"/> <visibleVertex name="vertex9" team="A"/> <visibleVertex name="vertex16" team="none"/> </visibleVertices> <visibleEdges> <visibleEdge node1="vertex1" node2="vertex7"/> <visibleEdge node1="vertex3" node2="vertex15"/> <visibleEdge node1="vertex15" node2="vertex19"/> <visibleEdge node1="vertex9" node2="vertex16"/> <visibleEdge node1="vertex4" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex9"/> <visibleEdge node1="vertex3" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex3"/> <visibleEdge node1="vertex9" node2="vertex11"/> <visibleEdge node1="vertex1" node2="vertex15"/> <visibleEdge node1="vertex9" node2="vertex12"/> <visibleEdge node1="vertex7" node2="vertex17"/> <visibleEdge node1="vertex2" node2="vertex7"/> <visibleEdge node1="vertex1" node2="vertex2"/> <visibleEdge node1="vertex9" node2="vertex19"/> <visibleEdge node1="vertex1" node2="vertex9"/> <visibleEdge node1="vertex7" node2="vertex12"/> </visibleEdges> <visibleEntities> <visibleEntity name="a9" node="vertex4" status="normal" team="A"/> <visibleEntity name="b2" node="vertex1" status="normal" team="B"/> <visibleEntity name="a5" node="vertex7" status="normal" team="A"/> <visibleEntity name="b7" node="vertex2" status="normal" team="B"/> <visibleEntity name="a6" node="vertex2" status="normal" team="A"/> <visibleEntity name="a2" node="vertex4" status="normal" team="A"/> <visibleEntity name="a10" node="vertex12" status="normal" team="A"/> <visibleEntity name="b5" node="vertex2" status="normal" team="B"/> <visibleEntity name="a3" node="vertex12" status="normal" team="A"/> <visibleEntity name="a1" node="vertex1" status="normal" team="A"/> <visibleEntity name="b4" node="vertex4" status="normal" team="B"/> <visibleEntity name="b8" node="vertex3" status="normal" team="B"/> </visibleEntities> </perception> </message>'

    print_message(parse(auth_response))
    print_message(parse(sim_start))
    parse(request_action1)
    parse(request_action2)
    parse(request_action3)
    print_message(parse(request_action4))
