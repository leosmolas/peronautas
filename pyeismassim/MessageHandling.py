from BeautifulSoup import BeautifulStoneSoup

sxmlh = u'<?xml version="1.0" encoding="UTF-8" standalone="no"?>'

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
    message_tag        = xml.contents[1]
    perception_tag     = message_tag.contents[0]
    simulation_tag     = perception_tag.contents[0]
    self_tag           = perception_tag.contents[1]
    team_tag           = perception_tag.contents[2]
    vis_verts_tag      = perception_tag.contents[3]
    vis_edges_tag      = perception_tag.contents[4]
    vis_ents_tag       = perception_tag.contents[5]
    probed_verts_tag   = perception_tag.contents[6]
    surveyed_edges_tag = perception_tag.contents[7]
    inspected_ents_tag = perception_tag.contents[8]
    achievements_tag   = team_tag.contents[0]

    result                        = {}
    result['timestamp']           = message_tag['timestamp']      
    result['type']                = message_tag['type']           
    result['deadline']            = perception_tag['deadline']    
    result['id']                  = perception_tag['id']          
    result['step']                = simulation_tag['step']        
    result['energy']              = self_tag['energy']            
    result['health']              = self_tag['health']            
    result['last_action']         = self_tag['lastAction']        
    result['last_action_result']  = self_tag['lastActionResult']  
    result['max_energy']          = self_tag['maxEnergy']         
    result['max_energy_disabled'] = self_tag['maxEnergyDisabled'] 
    result['max_health']          = self_tag['maxHealth']         
    result['position']            = self_tag['position']          
    result['strength']            = self_tag['strength']          
    result['vis_range']           = self_tag['visRange']          
    result['zone_score']          = self_tag['zoneScore']         
    result['last_step_score']     = team_tag['lastStepScore']     
    result['money']               = team_tag['money']             
    result['score']               = team_tag['score']             

    achievements_list = []
    for i in len(achievements_tag.contents):
        achievements_list.append(achievements_tag.contents[i]['name'])
    
    vis_verts_list = []
    for i in len(vis_verts_tag.contents):
        vis_vert = {}
        vis_vert['name'] = vis_verts_tag.contents[i]['name']
        vis_vert['team'] = vis_verts_tag.contents[i]['team']
        vis_verts_list.append(vis_vert)

    vis_edges_list = []
    for i in len(vis_edges_tag.contents):
        vis_edge = {}
        vis_edge['node1'] = vis_edges_tag.contents[i]['node1']
        vis_edge['node2'] = vis_edges_tag.contents[i]['node2']
        vis_edges_list.append(vis_edge)

    vis_ents_list = []
    for i in len(vis_ents_tag.contents):
        vis_ent = {}
        vis_ent['name'] = vis_ents_tag.contents[i]['name']
        vis_ent['node'] = vis_ents_tag.contents[i]['node']
        vis_ent['team'] = vis_ents_tag.contents[i]['team']
        vis_ents_list.append(vis_ent)

    probed_verts_list = []
    for i in len(probed_verts_tag.contents):
        probed_vert = {}
        probed_vert['name']  = probed_verts_tag.contents[i]['name']
        probed_vert['value'] = probed_verts_tag.contents[i]['value']
        probed_verts_list.append(probed_vert)

    surveyed_edges_list = []
    for i in len(surveyed_edges_tag.contents):
        surveyed_edge = {}
        surveyed_edge['node1']  = surveyed_edges_tag.contents[i]['node1']
        surveyed_edge['node2']  = surveyed_edges_tag.contents[i]['node2']
        surveyed_edge['weight'] = surveyed_edges_tag.contents[i]['weight']

    inspected_ents_list = []
    for i in len(inspected_ents_tag.contents):
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

    result['achievements']        = achievements_list
    result['vis_verts']           = vis_verts_list
    result['vis_edges']           = vis_edges_list
    result['vis_ents']            = vis_ents_list
    result['probed_verts']        = probed_verts_list
    result['survey_edges']        = surveyed_edges_list
    result['inspected_ents']      = inspected_ents_list

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

# Generation

def action_parry(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="parry"/></message>'

def action_probe(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="probe"/></message>'

def action_survey(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="survey"/></message>'

def action_inspect(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="inspect"/></message>'

def action_recharge(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="recharge"/></message>'

def action_skip(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="skip"/></message>'

def action_goto(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="goto" param="'   + param + '"/></message>'

def action_attack(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="attack" param="' + param + '"/></message>'

def action_repair(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="repair" param="' + param + '"/></message>'

def action_buy(action_id, param):
    return sxmlh + u'<message type="action"><action id="' + action_id + '" type="buy" param="'    + param + '"/></message>'

def action(action_id, action_type, action_parameter = None):
    action_functions = { "skip"     : action_skip,
                         "goto"     : action_goto,
                         "attack"   : action_attack,
                         "parry"    : action_parry,
                         "probe"    : action_probe,
                         "survey"   : action_survey,
                         "inspect"  : action_inspect,
                         "repair"   : action_repair,
                         "buy"      : action_buy,
                         "recharge" : action_recharge
                       }
    return action_functions[action_type](action_id, action_parameter)

def auth_request(username, password):
    return sxmlh + u'<message type="auth-request"><authentication password="' + password + '" username="' + username + '"/></message>'

def bye(timestamp):
    pass

def print_action_request(a):
    pass

auth_response  = u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message timestamp="1297263037617" type="auth-response"><authentication result="ok"></message>'
request_action = u''
sim_start      = u''
sim_end        = u''
print parse(auth_response)
#print parse(action_request)
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

