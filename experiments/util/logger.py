import numpy as np
import os
import os.path as op

def save_np_arr(save_path, file_name, np_arr):
	# type: str -> str -> np.array -> ()
	"""
	Save np_arr to <save_path/file_name.npy>.
	"""
	if not os.path.exists(save_path):
		os.makedirs(save_path)
	with open(os.path.join(save_path, file_name + '.npy'), 'w') as f:
		np.save(f, np_arr)

def load_np_arr(save_path, file_name):
	# type: str -> str -> ()
	"""
	Load array from <save_path/file_name.npy>.
	"""
	with open(os.path.join(save_path, file_name + '.npy'), 'r') as f:
		return np.load(f)
