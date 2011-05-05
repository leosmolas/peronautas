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
    print xml.contents[1].contents[0]

    message_tag    = xml.find('message')
    simulation_tag = xml.find('simulation')

    result              = {}
    result['timestamp'] = message_tag['timestamp']   
    result['type']      = message_tag['type']        
    result['edges']     = simulation_tag['edges']    
    result['id']        = simulation_tag['id']       
    result['steps']     = simulation_tag['steps']    
    result['vertices']  = simulation_tag['vertices'] 

    return result

def parse_sim_end(xml):

    message_tag    = xml.find('message')
    sim_result_tag = xml.find('sim-result')

    result              = {}
    result['timestamp'] = message_tag['timestamp']  
    result['type']      = message_tag['type']       
    result['ranking']   = sim_result_tag['ranking'] 
    result['score']     = sim_result_tag['score']   

    return result

def parse_bye(xml):
    message_tag         = xml.find('message')

    result              = {}
    result['timestamp'] = message_tag['timestamp']

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
        # This check is done because if the xml has no visible vertices tag, it will be None.
        achievements_list = []
        for i in range(len(achievements_tag.contents)):
            if (achievements_tag.contents[i].__class__.__name__ == "Tag"):
                # This check is done because the contents of the tag will be a list of objects which may be of class Tag or class NavigableString.
                # In the latter case, it is most probably a space or junk and will not be indexable with strings.
                achievements_list.append(achievements_tag.contents[i]['name'])
        result['achievements'] = achievements_list
    
    if (vis_verts_tag != None):
        vis_verts_list = []
        for i in range(len(vis_verts_tag.contents)):
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
                        "request-action" : parse_request_action,
                        "bye"            : parse_bye
                      }
    xml = BeautifulStoneSoup(msg, selfClosingTags=['authentication'])

    result = {}
    if (xml != None):
        message_tag  = xml.find('message')
        message_type = message_tag['type']
        result = parse_functions[message_type](xml)
    else:
        raise Exception
    return result

def print_message(result):
    """
    Use this function to pretty-print a parsed message.
    """
    # LIST COMPREHENSION MAGIC :D

    # This is a list comprehension; the part inside max() creates a list with the len function applied to every element in result.keys()
    max_key_length = max([len(k) for k in result.keys()])
    for key, value in sorted(result.iteritems()):
        # For every key,value pair in the dictionary:
        print "   ", key.ljust(max_key_length), ":",
        # If the value is a list with more than one element, it is most likely a list of dicts, and they all have the same set of keys. 
        # Except for the achievements, which is a list of strings. 
        # So recover one set of keys, calculate the length for each key, and use that as the column width for that key.
        if (value.__class__.__name__ == 'list' and (len(value) > 0)):
            if (value[0].__class__.__name__ == 'dict'):
                print
                # Esto es como dos for anidades que recorre primero todos los valores v1 en value, 
                # y luego cada clave k2 dentro de cada valor v1, y para cada clave k2 calcula la longitud de la clave, 
                # y se queda con el maximo entre todos.
                # Luego hace lo mismo para los valores en los valores de value.
                # Esto es para que todo quede alineado bonito y para que mi OCD me deje dormir de noche.
                key_column_width = max( [ max([ len(k2) for k2 in v1.keys()   ]) for v1 in value] )
                val_column_width = max( [ max([ len(v2) for v2 in v1.values() ]) for v1 in value] )
                for i in value:
                    print "       ", 
                    for k2, v2  in i.iteritems():
                        print k2.rjust(key_column_width), ":", v2.ljust(val_column_width), "|", 
                    print
            else:
                # It is a list of something else
                # TODO: pretty print list of something else
                print value
        else:
            print value

# Generation

def auth_request(username, password):
    return u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="auth-request"><authentication password="%s" username="%s"/></message>\0' % (password, username)

def bye():
    return u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="bye"/></message>\0'

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
        return u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="action"><action id="%s" type="%s"/></message>\0' % (action_id, action_type)
    else:
        return u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="action"><action id="%s" param="%s" type="%s"/></message>\0' % (action_id, action_parameter, action_type)

if (__name__ == "__main__"):
    #print auth_request("USER", "PASS")
    #print bye()
    #print action("14", "parry")
    #print action("14", "probe")
    #print action("14", "survey")
    #print action("14", "inspect")
    #print action("14", "recharge")
    #print action("14", "skip")
    #print action("14", "goto", "vertex1")
    #print action("14", "attack", "agent1")
    #print action("14", "repair", "agent1")
    #print action("14", "buy", "battery")

    print "AUTHENTICATION RESPONSE:"
    file = open('./testdata/auth_response.txt', 'r')
    for line in file:
        print_message(parse(line))
            
    print "SIMULATION START:"
    file = open('./testdata/sim_start.txt', 'r')
    for line in file:
        print_message(parse(line))

    print "ACTION REQUEST:"
    file = open('./testdata/action_requests.txt', 'r')
    for line in file:
        print_message(parse(line))
