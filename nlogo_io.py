import json
import os

'''
NETLOGO PARSING
'''

def nlogo_list_to_arr(list_str):
    return [ el.replace('[', '').strip().split(' ') for el in list_str[1:len(list_str)-1].split(']') ]

def nlogo_replace_agents(string, types):
    for t in types:
      string = string.replace(f'({t} ', f'{t}_')
    return string.replace(')','')

'''
Parse a NetLogo mixed dictionary into a Python dictionary. This is a nightmare.
But it works.

:param list_str: The NetLogo dictionary as a string.
'''
def nlogo_mixed_list_to_dict(list_str):
  return nlogo_parse_chunk(list_str)

def nlogo_mixed_list_to_dict_rec(list_str):
  # print(f'processing {list_str}')
  if list_str[0] == '[' and list_str[len(list_str)-1] == ']' and list_str.count('[') == 1:
    return nlogo_parse_chunk(list_str)

  d = {}
  chunk = ''
  stack_count = 0
  for i in range(1, len(list_str)-1):
    chunks = []
    char = list_str[i]
    chunk += char
    if char == '[':
      stack_count += 1
    elif char == ']':
      stack_count -= 1

      if stack_count == 0:
        # print(f'parsing chunk: {chunk}')
        parsed = nlogo_parse_chunk(chunk)
        # print(f'parsed: {parsed}')
        d[list(parsed.keys())[0]] = list(parsed.values())[0]
        chunk = ''
      # chunks[stack_count] += char
  return d

def nlogo_parse_chunk(chunk):
  chunk = chunk.strip().replace('"','')
  if chunk.count('[') > 1 and chunk[0] == '[':
    return nlogo_mixed_list_to_dict_rec(chunk[chunk.index('['):].strip())
  elif chunk.count('[') > 1 or chunk[0] != '[':
    return { chunk[0:chunk.index('[')].strip(): nlogo_mixed_list_to_dict_rec(chunk[chunk.index('['):].strip()) }

  pieces = chunk.strip().replace('[','').replace(']','').split(' ')
  if len(pieces) == 2:
    return { pieces[0]: pieces[1] }
  else:
    return pieces

def save_graph(path, cit, cit_social, media, media_sub):
  cit_arr = nlogo_list_to_arr(nlogo_replace_agents(cit, [ 'citizen' ]))
  cit_social_arr = nlogo_list_to_arr(nlogo_replace_agents(cit_social, [ 'citizen' ]))
  media_arr = nlogo_list_to_arr(nlogo_replace_agents(media, [ 'media' ]))
  media_sub_arr = nlogo_list_to_arr(nlogo_replace_agents(media_sub, [ 'media', 'citizen' ]))

  f = open(path, 'w')
  f.write(f'CITIZENS {len(cit_arr)-1}\n')
  for c in filter(lambda el: len(el) > 1, cit_arr):
    cit_id = c[0].replace("citizen_","")
    f.write(f'{cit_id},{c[1]},{c[3]}\n')

  f.write(f'CITIZEN_SOCIAL_LINKS {len(cit_social_arr)-1}\n')
  for c in filter(lambda el: len(el) > 1, cit_social_arr):
    f.write(f'{c[0].replace("citizen_","")},{c[1].replace("citizen_","")}\n')

  f.write(f'MEDIA {len(media_arr)-1}\n')
  for c in filter(lambda el: len(el) > 1, media_arr):
    f.write(f'{c[0].replace("media_","")},{c[1]},{c[2]}\n')

  f.write(f'MEDIA_SUB_LINKS {len(media_sub_arr)-1}\n')
  for c in filter(lambda el: len(el) > 1, media_sub_arr):
    ordered = sorted(c)
    f.write(f'{ordered[0].replace("citizen_","")},{ordered[1].replace("media_","")}\n')

  f.close()

def read_subrange(lines, into):
    for line in lines:
        into.append(line.split(','))

def read_graph(path):
    f = open(path, 'r')
    raw = f.read()
    f.close()
    cit = []
    cit_social = []
    media_arr = []
    media_sub_arr = []
    lines = raw.split('\n')
    i = 0

    while i < len(lines):
      line = lines[i]
      if 'CITIZEN_SOCIAL_LINKS' in line:
        n = int(line.split(' ')[1])
        read_subrange(lines[i+1:i+1+n], cit_social)
        i += n+1
      elif 'CITIZEN' in line:
        n = int(line.split(' ')[1])
        read_subrange(lines[i+1:i+1+n], cit)
        i += n+1
      elif 'MEDIA_SUB_LINKS' in line:
        n = int(line.split(' ')[1])
        read_subrange(lines[i+1:i+1+n], media_sub_arr)
        i += n+1
      elif 'MEDIA' in line:
        n = int(line.split(' ')[1])
        read_subrange(lines[i+1:i+1+n], media_arr)
        i += n+1
      else:
        i += 1
    return (cit, cit_social, media_arr, media_sub_arr)
  
def write_message_data(path, filename, bel_over_time, messages_heard, messages_believed, messages_sent):
  with open(f'{path}/{filename}_bel_over_time.json', 'w', encoding='utf-8') as f:
    json.dump(bel_over_time, f, ensure_ascii=False)
  with open(f'{path}/{filename}_messages_heard.json', 'w', encoding='utf-8') as f:
    json.dump(messages_heard, f, ensure_ascii=False)
  with open(f'{path}/{filename}_messages_believed.json', 'w', encoding='utf-8') as f:
    json.dump(messages_believed, f, ensure_ascii=False)
  with open(f'{path}/{filename}_messages_sent.json', 'w', encoding='utf-8') as f:
    json.dump(messages_sent, f, ensure_ascii=False)

def read_json_file(filename):
  data = None
  with open(filename, 'r') as f:
    data = json.load(f)
  f.close()
  return data

def read_cit_init_per_group(filename):
  return read_json_file(filename)

def get_all_world_files(top_path):
  path_to_ids = {}
  for root, dirs, files in os.walk(top_path):
    path = root.split(os.sep)
    path_name = '/'.join(path)
    file_ids = set([ f'{file.split("_")[0]}' for file in files if '_world.csv' in file ])
    # print(file_ids)
    if path_name not in path_to_ids and len(file_ids) > 0:
      path_to_ids[path_name] = []
    if len(file_ids) > 0:
      path_to_ids[path_name] = list(file_ids)
  return path_to_ids