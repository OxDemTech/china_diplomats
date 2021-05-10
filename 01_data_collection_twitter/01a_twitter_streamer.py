from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream
from tweepy import API
from datetime import datetime
import time
import os
import sys
import smtplib
from email.mime.text import MIMEText

import argparse


import pandas as pd
import numpy as np


## End of Settings###

class FileDumperListener(StreamListener):

    def __init__(self):
        super(FileDumperListener,self).__init__(self)
        #self.basePath=filepath
        #os.system("mkdir -p %s"%(filepath))

        d=datetime.today()
        self.filename = outputDir + "%i-%02d-%02d.json"%(d.year,d.month,d.day)              
        self.fh = open(self.filename,"a")#open for appending just in case
        
        self.tweetCount=0
        self.errorCount=0
        self.limitCount=0
        self.last=datetime.now()
    
    #This function gets called every time a new tweet is received on the stream
    def on_data(self, data):
        self.fh.write(data)
        self.tweetCount+=1
        
        #Status method prints out vitals every five minutes and also rotates the log if needed
        self.status()
        return True
        
    def close(self):
        try:
            self.fh.close()
        except:
            #Log/email
            pass

    #Rotate the log file if needed.
    #Warning: Check for log rotation only occurs when a tweet is received and not more than once every five minutes.
    #                 This means the log file could have tweets from a neighboring period (especially for sparse streams)
    def rotateFiles(self):
        d=datetime.today()
        filenow = outputDir + "%i-%02d-%02d.json"%(d.year,d.month,d.day)
        if (self.filename!=filenow):
            print("%s - Rotating log file. Old: %s New: %s"%(datetime.now(),self.filename,filenow))
            try:
                self.fh.close()
            except:
                #Log/Email it
                pass
            self.filename=filenow
            self.fh = open(self.filename,"a")

    def on_error(self, statusCode):
        print("%s - ERROR with status code %s"%(datetime.now(),statusCode))
        self.errorCount+=1
    
    def on_timeout(self):
        raise TimeoutException()
    
    def on_limit(self, track):
        print("%s - LIMIT message recieved %s"%(datetime.now(),track))
        self.limitCount+=1

    def status(self):
        now=datetime.now()
        if (now-self.last).total_seconds()>300:
            print("%s - %i tweets, %i limits, %i errors in previous five minutes."%(now,self.tweetCount,self.limitCount,self.errorCount))
            self.tweetCount=0
            self.limitCount=0
            self.errorCount=0
            self.last=now
            self.rotateFiles()#Check if file rotation is needed
            
            
def get_chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]


def parse_arguments():
    parser = argparse.ArgumentParser(description='Script to download and articles')
    parser.add_argument('--track',
                        help='Csv file with twitter_handle column holding accounts to track',
                        type=str,
                        required=True)
    parser.add_argument('--api_key',
                        help='which api key to use, options: prrrmp2, m_schliebs, ommsox01',
                        type=str,
                        required=True)
    parser.add_argument('--output_dir',
                        help='Output-Directory and date-prefix',
                        type=str,
                        required=True)
    parser.add_argument('--which',
                        help='Which streamer for email notifications',
                        type=str,
                        required=True)
    parser.add_argument('--include_mentions',
                        help='Which streamer for email notifications',
                        type=str,
                        required=True)
    parser.add_argument('--url_only',
                        help='xxxx',
                        type=str,
                        required=True)
    return parser.parse_args()


class TimeoutException(Exception):
    pass
    

if __name__ == '__main__':

    args = parse_arguments()

    
    if args.api_key == "xxxx":
        keys = xxxxx
    elif args.api_key == "xxxxx":
        keys = xxxxx
    elif args.api_key == "xxxxx":
        keys = xxxx
    elif args.api_key == "xxxxx":
        keys = xxxxxx
        
    consumer_key = keys['CONSUMER_KEY']
    consumer_secret = keys['CONSUMER_SECRET']
    access_token = keys['ACCESS_TOKEN']
    access_token_secret = keys['ACCESS_TOKEN_SECRET']
    
    track = args.track

    url_targets = pd.read_excel(open(track, 'rb'), sheet_name='url_targets',engine='openpyxl') 
              
    twitter_targets = pd.read_excel(open(track, 'rb'), sheet_name='twitter_targets',   engine='openpyxl') 


    # Output directory to hold json files (one per day) with tweets
    # Within the output directory, the script loads a file named FILTER with the terms to be tracked (one per line)

    #outputDir = "data/foreignstate-streaming-data/ms_live - "
    #outputDir = "data/ms/thesis_streamer/data/thesis_stream_"
    outputDir = args.output_dir
    
    #Email address to send error reports to


    while True:
        try:
            #Create the listener
            listener = FileDumperListener()
            auth = OAuthHandler(consumer_key, consumer_secret)
            auth.set_access_token(access_token, access_token_secret)

            handles = twitter_targets["twitter_handle"].dropna()
            
            atmentions = ['@'+x for x in handles]
            screen_handles = [x for x in handles]
            
            api = API(auth)
            
            user_lists = list(get_chunks(screen_handles,100))
            
            print(user_lists)
            
            user_ids = []
            
            for ll in user_lists:
                user_objects = api.lookup_users(screen_names = ll)
                part_ids = [user.id_str for user in user_objects]
                user_ids = user_ids + part_ids
                time.sleep(3)
                
         
            url_target_file = url_targets["twitter_keyword"].dropna()
            state_backed = [x for x in url_target_file]
            
            print("%s - Tracking number of URLs %s "%(datetime.now(),len(state_backed)))
            print("\n\n\n")
            
            print("%s - Tracking number of URLs %s \n\n %s"%(datetime.now(),len(state_backed),",".join(state_backed)))
            print("\n\n\n")
            
            url_terms = ['url:'+x for x in state_backed]
            
            if args.url_only == "Y":
                terms = url_terms
            if args.url_only == "N":
                terms = state_backed + url_terms
            
            if args.include_mentions == "Y":
                track_terms = atmentions  + terms
            if args.include_mentions == "N":
                track_terms = terms
            
            print("%s - IN total tracking number of terms: (max 400)  %s"%(datetime.now(),len(track_terms)))
            print("\n\n\n")
            print(track_terms)
            print("\n\n\n")
            #Connect to the Twitter stream
            stream = Stream(auth, listener)
            #we want to both get tweets / RTs from these people and track mentions
            
            stream.filter(follow = user_ids, track = track_terms)
            

        except KeyboardInterrupt:
            #User pressed ctrl+c or cmd+c -- get ready to exit the program
            print("%s - KeyboardInterrupt caught. Closing stream and exiting."%datetime.now())
            listener.close()
            stream.disconnect()
            break
            
        except TimeoutException:
            #Timeout error, network problems? reconnect.
            print("%s - Timeout exception caught. Closing stream and reopening."%datetime.now())
            try:
                listener.close()
                stream.disconnect()
            except:
                pass
            continue
        except:
            pass
            time.sleep(60)#Sleep thirty minutes and try again



