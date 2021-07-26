'''
Generic utilities file to keep track of useful functions.
'''

def dict_sort(d, reverse=False):
  return {key: value for key, value in sorted(d.items(), key=lambda item: item[1], reverse=reverse)}

def rgb_to_hex(rgb):
    return '%02x%02x%02x' % tuple(rgb)