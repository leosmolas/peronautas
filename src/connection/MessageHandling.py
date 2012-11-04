# -*- coding: utf-8 -*-

from BeautifulSoup import BeautifulStoneSoup
import pprint

# Author: Iñaki Garay
#
# Functions:
#   parse(xml_string, output)
#       parses a string that represents an xml message from the server
#       output specifies the type of output, and may be the string 'dict or the string 'prolog'
#   print_message(parsed_message, input)
#       pretty prints a parsed xml message
#       input is the type of the parsed message, and may be the string 'dict' or the string 'prolog'
#   auth_request(username, password)
#       generates a string that represents an autorization request xml message
#   bye()
#       generates a string that represents a goodbye xml message
#   action(action_id, action_type[, action_parameter])
#       generates a string that represents an action xml message

# Parsing

#------------------------------------------------------------------------------#
def parse_tournament(xml):
    tournament_tag = xml.find('tournament')
    team_tag       = xml.find('team')
    match_tag      = xml.find('match')
    result         = { 'result' : xml.prettify() }
    return ('tournament', None, result, None)
        
#------------------------------------------------------------------------------#
def parse_auth_response(xml):
    message_tag        = xml.find('message')
    authentication_tag = xml.find('authentication')
    result = { 'type'      : 'auth-response'
             , 'timestamp' : message_tag['timestamp']
             , 'result'    : authentication_tag['result']
             }
    return ('auth-response', None, result, None)

#------------------------------------------------------------------------------#
def parse_sim_start(xml):
    message_tag    = xml.find('message')
    simulation_tag = xml.find('simulation')
    result = { 'type'      : 'sim-start'
             , 'timestamp' : message_tag['timestamp']
             , 'edges'     : simulation_tag['edges']
             , 'id'        : simulation_tag['id']
             , 'steps'     : simulation_tag['steps']
             , 'role'      : simulation_tag['role']
             , 'vertices'  : simulation_tag['vertices']
             }
    return ('sim-start', None, result, None)

#------------------------------------------------------------------------------#
def parse_sim_end(xml):
    message_tag    = xml.find('message')
    sim_result_tag = xml.find('sim-result')
    result = { 'type'      : 'sim-end'
             , 'timestamp' : message_tag['timestamp']
             , 'ranking'   : sim_result_tag['ranking']
             , 'score'     : sim_result_tag['score']
             }
    return ('sim-end', None, result, None)

#------------------------------------------------------------------------------#
def parse_bye(xml):
    message_tag = xml.find('message')
    result = { 'type'      : 'bye'
             , 'timestamp' : message_tag['timestamp']
             }
    return ('bye', None, result, None)

#------------------------------------------------------------------------------#
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
    action_id          = perception_tag['id']
    
    result_private = { 'type'                : 'request-action'
                     , 'timestamp'           : message_tag['timestamp']
                     , 'deadline'            : perception_tag['deadline']
                     , 'total_time'          : (int(perception_tag['deadline']) - int(message_tag['timestamp']))
                     , 'id'                  : perception_tag['id']
                     , 'step'                : simulation_tag['step']
                     , 'energy'              : self_tag['energy']
                     , 'health'              : self_tag['health']
                     , 'last_action'         : self_tag['lastaction']
                     , 'last_action_result'  : self_tag['lastactionresult']
                     , 'max_energy'          : self_tag['maxenergy']
                     , 'max_energy_disabled' : self_tag['maxenergydisabled']
                     , 'max_health'          : self_tag['maxhealth']
                     , 'strength'            : self_tag['strength']
                     , 'vis_range'           : self_tag['visrange']
                     , 'zone_score'          : self_tag['zonescore']
                     , 'last_step_score'     : team_tag['laststepscore']
                     , 'money'               : team_tag['money']
                     , 'score'               : team_tag['score']
                     }
    result_public  = { 'position'  : [ { u'name'     : u'self'
                                       , u'node'     : self_tag['position']
                                       , 'vis_range' : self_tag['visrange']
                                       , 'health'    : self_tag['health']
                                       , 'max_health': self_tag['maxhealth']
                                       } ]
                     }

    # Achievements
    if (achievements_tag != None):
        # This check is done because if the xml has no achievements tag, it will
        # be None.
        achievements_list = []
        for i in range(len(achievements_tag.contents)):
            if (achievements_tag.contents[i].__class__.__name__ == "Tag"):
                # This check is done because the contents of the tag will be
                # a list of objects which may be of class Tag or class
                # NavigableString.
                # In the latter case, it is most probably a space or junk and
                # will not be indexable with strings.
                achievements_list.append("'%s'" % achievements_tag.contents[i]['name'])
        result_private['achievements'] = achievements_list
    else:
        result_private['achievements'] = []
    
    # Visible vertices.
    if (vis_verts_tag != None):
        vis_verts_list = []
        for i in range(len(vis_verts_tag.contents)):
            if (vis_verts_tag.contents[i].__class__.__name__ == "Tag"):
                vis_verts_list.append( { 'name' : "'%s'" % vis_verts_tag.contents[i]['name']
                                       , 'team' : "'%s'" % vis_verts_tag.contents[i]['team']
                                       } )
        result_public['vis_verts'] = vis_verts_list
    else:
        result_public['vis_verts'] = []

    # Visible edges.
    if (vis_edges_tag != None):
        vis_edges_list = []
        for i in range(len(vis_edges_tag.contents)):
            if (vis_edges_tag.contents[i].__class__.__name__ == "Tag"):
                vis_edges_list.append( { 'node1' : "'%s'" % vis_edges_tag.contents[i]['node1']
                                       , 'node2' : "'%s'" % vis_edges_tag.contents[i]['node2']
                                       } )
        result_public['vis_edges'] = vis_edges_list
    else:
        result_public['vis_edges'] = []

    # Visible entities.
    if (vis_ents_tag != None):
        vis_ents_list = []
        for i in range(len(vis_ents_tag.contents)):
            if (vis_ents_tag.contents[i].__class__.__name__ == "Tag"):
                vis_ents_list.append( { 'name'   : "'%s'" % vis_ents_tag.contents[i]['name']
                                      , 'node'   : "'%s'" % vis_ents_tag.contents[i]['node']
                                      , 'team'   : "'%s'" % vis_ents_tag.contents[i]['team']
                                      , 'status' : "'%s'" % vis_ents_tag.contents[i]['status'] # normal | disabled
                                      } )
        result_public['vis_ents'] = vis_ents_list
    else:
        result_public['vis_ents'] = []

    # Probed vertices.
    if (probed_verts_tag != None):
        probed_verts_list = []
        for i in range(len(probed_verts_tag.contents)):
            if (probed_verts_tag.contents[i].__class__.__name__ == "Tag"):
                probed_verts_list.append( { 'name'  : "'%s'" % probed_verts_tag.contents[i]['name']
                                          , 'value' :          probed_verts_tag.contents[i]['value']
                                          } )
        result_public['probed_verts'] = probed_verts_list
    else:
        result_public['probed_verts'] = []

    # Surveyed edges.
    if (surveyed_edges_tag != None):
        surveyed_edges_list = []
        for i in range(len(surveyed_edges_tag.contents)):
            if (surveyed_edges_tag.contents[i].__class__.__name__ == "Tag"):
                surveyed_edges_list.append( { 'node1'  : "'%s'" % surveyed_edges_tag.contents[i]['node1']
                                            , 'node2'  : "'%s'" % surveyed_edges_tag.contents[i]['node2']
                                            , 'weight' :          surveyed_edges_tag.contents[i]['weight']
                                            } )
        result_public['surveyed_edges'] = surveyed_edges_list
    else:
        result_public['surveyed_edges'] = []

    # Inspected entities.
    if (inspected_ents_tag != None):
        inspected_ents_list = []
        for i in range(len(inspected_ents_tag.contents)):
            if (inspected_ents_tag.contents[i].__class__.__name__ == "Tag"):
                inspected_ents_list.append( { 'energy'     :          inspected_ents_tag.contents[i]['energy']
                                            , 'health'     :          inspected_ents_tag.contents[i]['health']
                                            , 'max_energy' :          inspected_ents_tag.contents[i]['maxenergy']
                                            , 'max_health' :          inspected_ents_tag.contents[i]['maxhealth']
                                            , 'name'       : "'%s'" % inspected_ents_tag.contents[i]['name']
                                            , 'node'       : "'%s'" % inspected_ents_tag.contents[i]['node']
                                            , 'role'       : "'%s'" % inspected_ents_tag.contents[i]['role']
                                            , 'strength'   : "'%s'" % inspected_ents_tag.contents[i]['strength']
                                            , 'team'       : "'%s'" % inspected_ents_tag.contents[i]['team']
                                            , 'vis_range'  :          inspected_ents_tag.contents[i]['visrange']
                                            } )
        result_public['inspected_ents'] = inspected_ents_list
    else:
        result_public['inspected_ents'] = []
    return ('request-action', action_id, result_private, result_public)

#------------------------------------------------------------------------------#
def parse(msg):
    """The output parameter may be 'dict' or 'prolog'."""
    parse_functions = { "auth-response"  : parse_auth_response
                      , "sim-start"      : parse_sim_start
                      , "sim-end"        : parse_sim_end
                      , "request-action" : parse_request_action
                      , "tournament"     : parse_tournament
                      , "bye"            : parse_bye
                      }
    xml = BeautifulStoneSoup(msg, selfClosingTags=['authentication'])
    result = {}
    if (xml != None):
        message_tag  = xml.find('message')
        if (message_tag == None):
            result = parse_tournament(xml)
        else:
            message_type = message_tag['type']
            result = parse_functions[message_type](xml)
    else:
        raise Exception
    return result

#------------------------------------------------------------------------------#
def print_message(result):
    """
    Use this function to pretty-print a parsed message.
    """
    pp = pprint.PrettyPrinter(indent=4)
    pp.pprint(result)

# Generation

#------------------------------------------------------------------------------#
def auth_request(username, password):
    return u'''<?xml version="1.0" encoding="UTF-8" standalone="no"?> <message type="auth-request"> <authentication password="%s" username="%s"/> </message> \0''' % (password, username)

#------------------------------------------------------------------------------#
def bye():
    return u'<?xml version="1.0" encoding="UTF-8" standalone="no"?><message type="bye"/></message>\0'

#------------------------------------------------------------------------------#
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

