#!/usr/bin/env python

### In order to set the access key and secret key, 
### you need to have ~/.aws/credentials and 
### AWS_CREDENTIAL_FILE exported as an environmental variable
### before running this code. 

import math, os
import boto
from boto.s3.key import Key
from filechunkio import FileChunkIO
#from boto.s3.connection import S3Connection

c = boto.connect_s3()
b = c.get_bucket('census-cdr', validate=False)

#k = b.new_key('mi-to-mi')

def mycb(so_far, total):
        print ('%d bytes transferred out of %d' %(so_far, total))


# Get file info
source_path = '/home/ubuntu/1556484/cdr/cdr-dec.zip'
source_size = os.stat(source_path).st_size

# Create a multipart upload request
mp = b.initiate_multipart_upload(os.path.basename(source_path))

# Use a chunk size of 5 GB (feel free to change this)
chunk_size = 5368709120
chunk_count = int(math.ceil(source_size / float(chunk_size)))

# Send the file parts, using FileChunkIO to create a file-like object
# that points to a certain byte range within the original file. We
# set bytes to never exceed the original file size.
for i in range(chunk_count):
        offset = chunk_size * i
        bytes = min(chunk_size, source_size - offset)
        with FileChunkIO(source_path, 'r', offset=offset, bytes=bytes) as fp:
                mp.upload_part_from_file(fp, part_num=i + 1, cb=mycb)

# Finish the upload
mp.complete_upload()
