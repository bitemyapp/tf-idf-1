# Cosine similarity

def magnitude(x): pass

def dot_product(v1, v2):
    return sum(x*y for x,y in zip(v1, v2))

def cosine_similarity(x,y): pass

def norm(a):
  n = len(a)
  for i in xrange(n):
    sum += a[i] * a[i]
  return math.sqrt(sum)

def cossim(a,b):
  return dot(a,b) / (norm(a) * norm(b))
