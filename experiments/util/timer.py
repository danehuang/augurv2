import StringIO
import time
import cProfile
import pstats



class Timer:    
	def __enter__(self):
		self.start = time.clock()
		return self

	def __exit__(self, *args):
		self.end = time.clock()
		self.interval = self.end - self.start



#-------------------------------------------------------------
# Timing functions
#-------------------------------------------------------------

def parse_time(stream, fn_name):
	for line in stream:
		if line.find('(' + fn_name + ')') != -1:
			arr = line.strip().split()
			return arr[3]
	return None

def get_time_sys(file_name, fn_name):
	stream = StringIO.StringIO()
	p = pstats.Stats(file_name, stream=stream)
	p.strip_dirs().sort_stats(-1).print_stats()
	return parse_time(stream.getvalue().split('\n'), fn_name)