# -*- coding: utf-8 -*-

from BeautifulSoup import BeautifulStoneSoup
from string import replace, lower

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
def parse_tournament(xml, output = 'dict'):
    tournament_tag = xml.find('tournament')
    team_tag       = xml.find('team')
    match_tag      = xml.find('match')

#<?xml version="1.0" encoding="UTF-8"?>
#<tournament tournament-name="Mars2011">
#<team name="A"/><team name="B"/>
#<match blue="B" red="A"/>
#</tournament>

    if (output == 'dict'):
        result = {
                'result' : xml.prettify()
                }
    elif (output == 'prolog'):
        result = [
                xml.prettify()
                ]
    else:
        raise Exception
    return ('tournament', None, result, None)
        
#------------------------------------------------------------------------------#
def parse_auth_response(xml, output = 'dict'):
    message_tag        = xml.find('message')
    authentication_tag = xml.find('authentication')

    if (output == 'dict'):
        result = {
            'type'      : 'auth-response'              ,
            'timestamp' : message_tag['timestamp']     ,
            'result'    : authentication_tag['result'] }
    elif (output == 'prolog'):
        result = [
            'type(auth_response)'                                   ,
            'timestamp(%s)' % message_tag['timestamp']              ,
            'result(%s)'    % authentication_tag['result']          ]
    else:
        raise Exception
    return ('auth-response', None, result, None)

#------------------------------------------------------------------------------#
def parse_sim_start(xml, output = 'dict'):
    message_tag    = xml.find('message')
    simulation_tag = xml.find('simulation')

    if (output == 'dict'):
        result = {
            'type'      : 'sim-start'                ,
            'timestamp' : message_tag['timestamp']   ,
            'edges'     : simulation_tag['edges']    ,
            'id'        : simulation_tag['id']       ,
            'steps'     : simulation_tag['steps']    ,
            'role'      : simulation_tag['role']     ,
            'vertices'  : simulation_tag['vertices'] }
    elif (output == "prolog"):
        result = [
            'type(sim_start)'                            ,
            'timestamp(%s)' % message_tag['timestamp']   ,
            'edges(%s)'     % simulation_tag['edges']    ,
            'id(%s)'        % simulation_tag['id']       ,
            'steps(%s)'     % simulation_tag['steps']    ,
            'role(%s)'      % simulation_tag['role']     ,
            'vertices(%s)'  % simulation_tag['vertices'] ]
    else:
        raise Exception
    return ('sim-start', None, result, None)

#------------------------------------------------------------------------------#
def parse_sim_end(xml, output = 'dict'):
    message_tag    = xml.find('message')
    sim_result_tag = xml.find('sim-result')

    if (output == "dict"):
        result = {
            'type'      : 'sim-end'                 ,
            'timestamp' : message_tag['timestamp']  ,
            'ranking'   : sim_result_tag['ranking'] ,
            'score'     : sim_result_tag['score']   }
    elif (output == "prolog"):
        result = [
            'type(sim_end)'                             ,
            'timestamp(%s)' % message_tag['timestamp']  ,
            'ranking(%s)'   % sim_result_tag['ranking'] ,
            'score(%s)'     % sim_result_tag['score']   ]
    else:
        raise Exception
    return ('sim-end', None, result, None)

#------------------------------------------------------------------------------#
def parse_bye(xml, output = 'dict'):
    message_tag = xml.find('message')
    if (output == 'dict'):
        result = { 
            'type' : 'bye'                         ,
            'timestamp' : message_tag['timestamp'] }
    elif (output == 'prolog'):
        result = [ 
            'type(bye)'                                ,
            'timestamp(%s)' % message_tag['timestamp'] ]
    else:
        raise Exception
    return ('bye', None, result, None)

#------------------------------------------------------------------------------#
def parse_request_action(xml, output = 'dict'):
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
    
    if (output == "dict"):
        result_private = {
            'type'                : 'request-action'             ,
            'timestamp'           : message_tag['timestamp']     ,
            'deadline'            : perception_tag['deadline']   ,
            'id'                  : perception_tag['id']         ,
            'step'                : simulation_tag['step']       ,
            'energy'              : self_tag['energy']           ,
            'health'              : self_tag['health']           ,
            'last_action'         : self_tag['lastaction']       ,
            'last_action_result'  : self_tag['lastactionresult'] ,
            'max_energy'          : self_tag['maxenergy']        ,
            'max_energy_disabled' : self_tag['maxenergydisabled'],
            'max_health'          : self_tag['maxhealth']        ,
            'strength'            : self_tag['strength']         ,
            'vis_range'           : self_tag['visrange']         ,
            'zone_score'          : self_tag['zonescore']        ,
            'last_step_score'     : team_tag['laststepscore']    ,
            'money'               : team_tag['money']            ,
            'score'               : team_tag['score']            
            }
        result_public = { 'position' : [{ 'name' : 'self', 'node' : self_tag['position'] }] }

        if (achievements_tag != None):
            # This check is done because if the xml has no visible vertices tag, it will be None.
            achievements_list = []
            for i in range(len(achievements_tag.contents)):
                if (achievements_tag.contents[i].__class__.__name__ == "Tag"):
                    # This check is done because the contents of the tag will be a list of objects which may be of class Tag or class NavigableString.
                    # In the latter case, it is most probably a space or junk and will not be indexable with strings.
                    achievements_list.append(achievements_tag.contents[i]['name'])
            result_private['achievements'] = achievements_list
        else:
            result_private['achievements'] = []
        
        if (vis_verts_tag != None):
            vis_verts_list = []
            for i in range(len(vis_verts_tag.contents)):
                if (vis_verts_tag.contents[i].__class__.__name__ == "Tag"):
                    vis_verts_list.append({ 
                        'name' : vis_verts_tag.contents[i]['name']         ,
                        'team' : vis_verts_tag.contents[i]['team'].lower() })
            result_public['vis_verts'] = vis_verts_list
        else:
            result_public['vis_verts'] = []

        if (vis_edges_tag != None):
            vis_edges_list = []
            for i in range(len(vis_edges_tag.contents)):
                if (vis_edges_tag.contents[i].__class__.__name__ == "Tag"):
                    vis_edges_list.append({ 
                        'node1' : vis_edges_tag.contents[i]['node1'] ,
                        'node2' : vis_edges_tag.contents[i]['node2'] })
            result_public['vis_edges'] = vis_edges_list
        else:
            result_public['vis_edges'] = []

        if (vis_ents_tag != None):
            vis_ents_list = []
            for i in range(len(vis_ents_tag.contents)):
                if (vis_ents_tag.contents[i].__class__.__name__ == "Tag"):
                    vis_ents_list.append({ 
                        'name'   : vis_ents_tag.contents[i]['name']        ,
                        'node'   : vis_ents_tag.contents[i]['node']        ,
                        'team'   : vis_ents_tag.contents[i]['team'].lower(),
                        'status' : vis_ents_tag.contents[i]['status']      }) # normal | disabled
            result_public['vis_ents'] = vis_ents_list
        else:
            result_public['vis_ents'] = []

        if (probed_verts_tag != None):
            probed_verts_list = []
            for i in range(len(probed_verts_tag.contents)):
                if (probed_verts_tag.contents[i].__class__.__name__ == "Tag"):
                    probed_verts_list.append({ 
                        'name'  : probed_verts_tag.contents[i]['name']  ,
                        'value' : probed_verts_tag.contents[i]['value'] })
            result_public['probed_verts'] = probed_verts_list
        else:
            result_public['probed_verts'] = []

        if (surveyed_edges_tag != None):
            surveyed_edges_list = []
            for i in range(len(surveyed_edges_tag.contents)):
                if (surveyed_edges_tag.contents[i].__class__.__name__ == "Tag"):
                    surveyed_edges_list.append({ 
                        'node1'  : surveyed_edges_tag.contents[i]['node1']  ,
                        'node2'  : surveyed_edges_tag.contents[i]['node2']  ,
                        'weight' : surveyed_edges_tag.contents[i]['weight'] })
            result_public['surveyed_edges'] = surveyed_edges_list
        else:
            result_public['surveyed_edges'] = []

        if (inspected_ents_tag != None):
            inspected_ents_list = []
            for i in range(len(inspected_ents_tag.contents)):
                if (inspected_ents_tag.contents[i].__class__.__name__ == "Tag"):
                    inspected_ents_list.append({ 
                        'energy'     : inspected_ents_tag.contents[i]['energy']       ,
                        'health'     : inspected_ents_tag.contents[i]['health']       ,
                        'max_energy' : inspected_ents_tag.contents[i]['max_energy']   ,
                        'max_health' : inspected_ents_tag.contents[i]['max_health']   ,
                        'name'       : inspected_ents_tag.contents[i]['name']         ,
                        'node'       : inspected_ents_tag.contents[i]['node']         ,
                        'role'       : inspected_ents_tag.contents[i]['role']         ,
                        'strength'   : inspected_ents_tag.contents[i]['strength']     ,
                        'team'       : inspected_ents_tag.contents[i]['team'].lower() ,
                        'vis_range'  : inspected_ents_tag.contents[i]['vis_range']    })
            result_public['inspected_ents'] = inspected_ents_list
        else:
            result_public['inspected_ents'] = []
            
    # Cruft.
    elif (output == "prolog"):
        result_private = [
            'type(request_action)'                                    ,
            'timestamp(%s)'           % message_tag['timestamp']      ,
            'deadline(%s)'            % perception_tag['deadline']    ,
            'id(%s)'                  % perception_tag['id']          ,
            'step(%s)'                % simulation_tag['step']        ,
            'energy(%s)'              % self_tag['energy']            ,
            'health(%s)'              % self_tag['health']            ,
            'last_action(%s)'         % self_tag['lastaction']        ,
            'last_action_result(%s)'  % self_tag['lastactionresult']  ,
            'max_energy(%s)'          % self_tag['maxenergy']         ,
            'max_energy_disabled(%s)' % self_tag['maxenergydisabled'] ,
            'max_health(%s)'          % self_tag['maxhealth']         ,
            'strength(%s)'            % self_tag['strength']          ,
            'vis_range(%s)'           % self_tag['visrange']          ,
            'zone_score(%s)'          % self_tag['zonescore']         ,
            'last_step_score(%s)'     % team_tag['laststepscore']     ,
            'money(%s)'               % team_tag['money']             ,
            'score(%s)'               % team_tag['score']             
            ]
        result_public = [
            'position(%s)'            % self_tag['position']
            ]

        if (achievements_tag != None):
            # This check is done because if the xml has no visible vertices tag, it will be None.
            achievements_list = []
            for i in range(len(achievements_tag.contents)):
                if (achievements_tag.contents[i].__class__.__name__ == "Tag"):
                    # This check is done because the contents of the tag will be a list of objects which may be of class Tag or class NavigableString.
                    # In the latter case, it is most probably a space or junk and will not be indexable with strings.
                    achievements_list.append('achievement(%s)' % achievements_tag.contents[i]['name'])
            result_private += achievements_list
        
        if (vis_verts_tag != None):
            vis_verts_list = []
            for i in range(len(vis_verts_tag.contents)):
                if (vis_verts_tag.contents[i].__class__.__name__ == "Tag"):
                    vis_verts_list.append('vis_vert(%s,%s)' % (vis_verts_tag.contents[i]['name'], 
                                                               vis_verts_tag.contents[i]['team'].lower()))
            result_public += vis_verts_list

        if (vis_edges_tag != None):
            vis_edges_list = []
            for i in range(len(vis_edges_tag.contents)):
                if (vis_edges_tag.contents[i].__class__.__name__ == "Tag"):
                    vis_edges_list.append('vis_edge(%s,%s)' % (vis_edges_tag.contents[i]['node1'], 
                                                               vis_edges_tag.contents[i]['node2']))
            result += vis_edges_list

        if (vis_ents_tag != None):
            vis_ents_list = []
            for i in range(len(vis_ents_tag.contents)):
                if (vis_ents_tag.contents[i].__class__.__name__ == "Tag"):
                    vis_ents_list.append('vis_ent(%s,%s,%s)' % (vis_ents_tag.contents[i]['name'], 
                                                                vis_ents_tag.contents[i]['node'], 
                                                                vis_ents_tag.contents[i]['team'].lower()))
            result_public += vis_ents_list

        if (probed_verts_tag != None):
            probed_verts_list = []
            for i in range(len(probed_verts_tag.contents)):
                if (probed_verts_tag.contents[i].__class__.__name__ == "Tag"):
                    probed_verts_list.append('probed_vert(%s,%s)' % (probed_verts_tag.contents[i]['name'], 
                                                                     probed_verts_tag.contents[i]['value']))
            result_public += probed_verts_list

        if (surveyed_edges_tag != None):
            surveyed_edges_list = []
            for i in range(len(surveyed_edges_tag.contents)):
                if (surveyed_edges_tag.contents[i].__class__.__name__ == "Tag"):
                    surveyed_edges_list.append('surveyed_edge(%s,%s,%s)' % (surveyed_edges_tag.contents[i]['node1'], 
                                                                            surveyed_edges_tag.contents[i]['node2'], 
                                                                            surveyed_edges_tag.contents[i]['weight']))
            result_public += surveyed_edges_list

        if (inspected_ents_tag != None):
            inspected_ents_list = []
            for i in range(len(inspected_ents_tag.contents)):
                if (inspected_ents_tag.contents[i].__class__.__name__ == "Tag"):
                    inspected_ents_list.append('inspected_ent(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)' % (inspected_ents_tag.contents[i]['energy'], 
                                                                                                 inspected_ents_tag.contents[i]['health'], 
                                                                                                 inspected_ents_tag.contents[i]['max_energy'], 
                                                                                                 inspected_ents_tag.contents[i]['max_health'], 
                                                                                                 inspected_ents_tag.contents[i]['name'], 
                                                                                                 inspected_ents_tag.contents[i]['node'], 
                                                                                                 inspected_ents_tag.contents[i]['role'], 
                                                                                                 inspected_ents_tag.contents[i]['strength'], 
                                                                                                 inspected_ents_tag.contents[i]['team'].lower(), 
                                                                                                 inspected_ents_tag.contents[i]['vis_range']))
            result_public += inspected_ents_list
    else:
        raise Exception
    return ('request-action', action_id, result_private, result_public)

#------------------------------------------------------------------------------#
def parse_as_dict(msg):
    return parse(msg, "dict")

#------------------------------------------------------------------------------#
def parse_as_list(msg):
    return parse(msg, 'prolog')

#------------------------------------------------------------------------------#
def parse(msg, output):
    """The output parameter may be 'dict' or 'prolog'."""
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
        if (message_tag == None):
            result = parse_tournament(xml, output)
        else:
            message_type = message_tag['type']
            result = parse_functions[message_type](xml, output)
    else:
        raise Exception
    return result

#------------------------------------------------------------------------------#
def print_message_dict(result):
    print_message(result, 'dict')

#------------------------------------------------------------------------------#
def print_message_list(result):
    print_message(result, 'prolog')

#------------------------------------------------------------------------------#
def print_message(result, input = 'dict'):
    """
    Use this function to pretty-print a parsed message.
    """

    if (input == 'dict'):
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
    elif (input == 'prolog'):
        for predicate in result:
            print predicate
    else:
        raise Exception

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

#------------------------------------------------------------------------------#
if (__name__ == "__main__"):
    print "AUTHENTICATION RESPONSE:"
    file = open('./testdata/auth_response.txt', 'r')
    for line in file:
        print_message(parse(line, 'prolog'), 'prolog')
            
    print "SIMULATION START:"
    file = open('./testdata/sim_start.txt', 'r')
    for line in file:
        print_message(parse(line, 'prolog'), 'prolog')

    print "ACTION REQUEST:"
    file = open('./testdata/action_requests.txt', 'r')
    for line in file:
        print_message(parse(line, 'prolog'), 'prolog')
