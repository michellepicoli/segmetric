LEM_multiresolution

Segmentation (Ecognition)
original segmentation: multires_bands_500_0.9_0.1_spectraldiff_band3_20
segmentation algorithm: multiresolution
scale: 500
shape: 0.9
compactness: 0.1
spectral difference: 20

Post processing (QGis)
was selected only the intersection of the segments with the reference data
simplify: distance (Douglas-Peucker) - tolerance: 10 meters
to eliminate the intersection: SAGA - Polygon Self-Intersection
