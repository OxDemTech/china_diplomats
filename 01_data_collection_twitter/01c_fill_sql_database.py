import pandas as pd
import json
import datetime
import glob

import psycopg2
dbname = "xxx"
connection = psycopg2.connect(user = 'xxxx',password = 'xxxx', database = dbname, host = 'xxxxxx', port = "xxxxxx")

in_files = glob.glob("PATH TO TWEET JSONS")

in_files
len(in_files)

class tweet:
  
    # initialize
  def __init__(self, json):
      
      # basic tweet level attributes
    self.tweet = json
    self.tweet_keys = json.keys()
    #self.timestamp = datetime.datetime.utcfromtimestamp(int(json['timestamp_ms'])/1000).strftime("%Y-%m-%d %H:%M:%S") + "+00" if 'timestamp_ms' in self.tweet_keys else None
    self.timestamp = datetime.datetime.utcfromtimestamp(int(json['timestamp_ms'])/1000).strftime("%Y-%m-%d %H:%M:%S") if 'timestamp_ms' in self.tweet_keys else None
    self.tweet_id = json["id"] if 'id' in self.tweet_keys else None
    self.tweet_id_str = json["id"] if 'id_str' in self.tweet_keys else None
    
    # Get Attributes to decide handling-forks
    self.truncated = True if ('truncated' in self.tweet_keys and json["truncated"] == True) else False
   
    # For all 
    self.user = json['user']
    self.user_keys = json['user'].keys()
    
    # double check here, there are tweets that are denoted as quoted but do not carry quoted object
    self.is_quote_status = True if (json['is_quote_status'] == True) and ('quoted_status' in self.tweet_keys) else False
    self.is_retweeted_status = True if ('retweeted_status' in self.tweet_keys) else False

    #if truncated
    if self.truncated == False:
      self.tweet_entities = json["entities"]
    elif self.truncated == True:
      self.tweet_entities = json['extended_tweet']['entities']
    
    # if it is quoted tweet
    if self.is_quote_status == True:
      self.quoted_status = json["quoted_status"]
      self.quoted_user = json["quoted_status"]['user']
      if json["quoted_status"]['truncated'] == False:
        self.quoted_entities = json['quoted_status']['entities']
      else:
        self.quoted_entities = json['quoted_status']['extended_tweet']['entities']
  
    # if it is retweet
    if self.is_retweeted_status:
      self.retweeted_status = json["retweeted_status"]
      self.retweeted_user = json["retweeted_status"]['user']
      if json["retweeted_status"]['truncated'] == False:
        self.retweeted_entities = json['retweeted_status']['entities']
      else:
        self.retweeted_entities = json['retweeted_status']['extended_tweet']['entities']
       
      
  def save_tweet_static(self,is_retweeted = False, is_quoted = False,table = "tweets_static",data_source = "streamer",dbtype = "mysql"):
      
    tweet_keys = self.tweet.keys()
    is_retweeting = True if 'retweeted_status' in tweet_keys else False
    is_quoting = True if 'quoted_status' in tweet_keys else False
    
    if (is_retweeted == False) and (is_quoted == False):
      tweet_id = self.tweet_id
      tweet_id_str = self.tweet_id_str,
      full_text = self.tweet['extended_tweet']['full_text'] if self.truncated else self.tweet['text']
      source = self.tweet["source"] if 'source' in tweet_keys else None
      tweet_created_at_raw = self.tweet["created_at"] if 'created_at' in tweet_keys else None
      tweet_created_at = datetime.datetime.strptime(self.tweet["created_at"], 
                                                    '%a %b %d %H:%M:%S %z %Y').strftime("%Y-%m-%d %H:%M:%S") #+ "+00"#
      user_id = self.tweet['user']['id']
      in_reply_to_status_id = self.tweet['in_reply_to_status_id']
      in_reply_to_user_id = self.tweet['in_reply_to_user_id']
      in_reply_to_screen_name = self.tweet['in_reply_to_screen_name']
      retweet_id = self.tweet['retweeted_status']['id'] if is_retweeting == True else None
      quoted_id = self.tweet["quoted_status_id"] if (is_quoting == True) and (is_retweeting == False) else None
      is_retweeting = is_retweeting
      is_quoting = True if (is_quoting == True) and (is_retweeting == False) else False
      
    if (is_retweeted == True) and (is_quoted == False):
      retweet_keys = self.retweeted_status.keys()
      tweet_id = self.retweeted_status['id']
      tweet_id_str = self.retweeted_status['id_str'],
      full_text = self.retweeted_status['extended_tweet']['full_text'] if self.retweeted_status["truncated"] == True else self.retweeted_status['text']
      source = self.retweeted_status["source"] if 'source' in retweet_keys else None
      tweet_created_at_raw = self.retweeted_status["created_at"] if 'created_at' in retweet_keys else None
      tweet_created_at = datetime.datetime.strptime(self.retweeted_status["created_at"], 
                                                    '%a %b %d %H:%M:%S %z %Y').strftime("%Y-%m-%d %H:%M:%S")# + "+00"#
      user_id = self.retweeted_status['user']['id']
      in_reply_to_status_id = self.retweeted_status['in_reply_to_status_id']
      in_reply_to_user_id = self.tweet['in_reply_to_user_id']
      in_reply_to_screen_name = self.tweet['in_reply_to_screen_name']
      retweet_id = None 
      quoted_id = self.retweeted_status["quoted_status_id"] if 'quoted_status_id' in retweet_keys else None
      is_retweeting = False
      is_quoting = True if 'quoted_status_id' in retweet_keys else False
      
    if (is_retweeted == False) and (is_quoted == True):
      retweet_keys = self.quoted_status.keys()
      tweet_id = self.quoted_status['id']
      tweet_id_str = self.quoted_status['id_str'],
      full_text = self.quoted_status['extended_tweet']['full_text'] if self.quoted_status["truncated"] == True else self.quoted_status['text']
      source = self.quoted_status["source"] if 'source' in retweet_keys else None
      tweet_created_at_raw = self.quoted_status["created_at"] if 'created_at' in retweet_keys else None
      tweet_created_at = datetime.datetime.strptime(self.quoted_status["created_at"], 
                                                    '%a %b %d %H:%M:%S %z %Y').strftime("%Y-%m-%d %H:%M:%S") #+ "+00"#
      user_id = self.quoted_status['user']['id']
      in_reply_to_status_id = self.quoted_status['in_reply_to_status_id']
      in_reply_to_user_id = self.tweet['in_reply_to_user_id']
      in_reply_to_screen_name = self.tweet['in_reply_to_screen_name']
      retweet_id = None
      quoted_id = None 
      is_retweeting = False
      is_quoting = False
        
    if dbtype == "postgres":
      cursor.execute(f'''
      INSERT INTO {table} (tweet_id, tweet_id_str,full_text,source,
      tweet_created_at,user_id,is_retweeted,is_quoted,
      is_retweeting,is_quoting,in_reply_to_status_id,in_reply_to_user_id,in_reply_to_screen_name,retweet_id,quoted_id,data_source) 
      VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
      ON CONFLICT DO NOTHING''',
      (tweet_id,tweet_id_str,full_text,source,tweet_created_at,user_id,
       is_retweeted,is_quoted,is_retweeting,
       is_quoting,in_reply_to_status_id,in_reply_to_user_id,in_reply_to_screen_name,retweet_id,quoted_id,data_source)
      )        
        
    if dbtype == "mysql":
      cursor.execute(f'''
      INSERT IGNORE INTO {table} (tweet_id, tweet_id_str,full_text,source,
      tweet_created_at,user_id,is_retweeted,is_quoted,
      is_retweeting,is_quoting,in_reply_to_status_id,in_reply_to_user_id,in_reply_to_screen_name,retweet_id,quoted_id,data_source) 
      VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)''',
      (tweet_id,tweet_id_str,full_text,source,tweet_created_at,user_id,
       is_retweeted,is_quoted,is_retweeting,
       is_quoting,in_reply_to_status_id,in_reply_to_user_id,in_reply_to_screen_name,retweet_id,quoted_id,data_source)
      )  
      
  def save_tweet_dynamic(self,is_retweeted = False, is_quoted = False,table = "tweets_dynamic",data_source = "streamer"):

    tweet_keys = self.tweet.keys()
    is_retweeted = is_retweeted
    is_quoted = is_quoted
    
    if (is_retweeted == False) and (is_quoted == False):
      tweet_id = self.tweet_id
      quote_count = self.tweet['quote_count']
      reply_count = self.tweet['reply_count']
      retweet_count = self.tweet['retweet_count']
      favorite_count = self.tweet['favorite_count']
      
    if is_retweeted == True: 
      tweet_id = self.retweeted_status['id']
      quote_count = self.retweeted_status['quote_count']
      reply_count = self.retweeted_status['reply_count']
      retweet_count = self.retweeted_status['retweet_count']
      favorite_count = self.retweeted_status['favorite_count']            
      
    if is_quoted == True: 
      tweet_id = self.quoted_status['id']
      quote_count = self.quoted_status['quote_count']
      reply_count = self.quoted_status['reply_count']
      retweet_count = self.quoted_status['retweet_count']
      favorite_count = self.quoted_status['favorite_count']              

    cursor.execute(f'''
    INSERT INTO {table} (tweet_id,quote_count,reply_count,retweet_count,favorite_count,timestamp_observed,data_source) VALUES (%s,%s,%s,%s,%s,%s,%s)
    ''',
    (tweet_id,quote_count,reply_count,retweet_count,favorite_count,self.timestamp,data_source)
    )
      
      
  def save_user_static(self,user,cursor,table = "users_static",data_source = "streamer",dbtype = "mysql"):
      
    user_keys = user.keys()
    user_id = user["id"] if 'id' in user_keys else None
    user_id_str = user["id_str"] if 'id_str' in user_keys else None
    
    if dbtype == "postgres":
      cursor.execute(f'''
      INSERT INTO {table} (user_id, user_id_str,data_source) VALUES (%s,%s,%s)
      ON CONFLICT DO NOTHING;
      ''',
      (user_id,user_id_str,data_source)
      )
      
    elif dbtype == "mysql":
      cursor.execute(f'''
      INSERT IGNORE INTO {table} (user_id, user_id_str,data_source) VALUES (%s,%s,%s)
      ''',
      (user_id,user_id_str,data_source)
      )
    
      
  def save_user_dynamic(self,user,cursor,table = "users_dynamic",data_source = "streamer"):
      
    user_keys = user.keys()
    user_id = user["id"] if 'id' in user_keys else None
    user_name = user["name"] if 'name' in user_keys else None
    user_screen_name = user["screen_name"] if 'screen_name' in user_keys else None
    user_location = user["location"] if 'location' in user_keys else None
    user_created_at_raw = user["created_at"] if 'created_at' in user_keys else ""
    #user_created_at = datetime.datetime.strptime(user["created_at"], '%a %b %d %H:%M:%S %z %Y').strftime("%Y-%m-%d %H:%M:%S") + "+00"#
    user_created_at = datetime.datetime.strptime(user["created_at"], '%a %b %d %H:%M:%S %z %Y').strftime("%Y-%m-%d %H:%M:%S")
    user_url = user["url"] if 'url' in user_keys else None
    user_description = user["description"] if 'description' in user_keys else None
    user_protected = user["protected"] if 'protected' in user_keys else None
    user_verified = user["verified"] if 'verified' in user_keys else None
    user_geo_enabled = user["geo_enabled"] if 'geo_enabled' in user_keys else None
    user_lang = user["lang"] if 'statuslanges_count' in user_keys else None
    #add more
    user_followers_count = user["followers_count"] if 'followers_count' in user_keys else None
    user_friends_count = user["friends_count"] if 'friends_count' in user_keys else None
    user_favourites_count = user["favourites_count"] if 'favourites_count' in user_keys else None
    user_statuses_count = user["statuses_count"] if 'statuses_count' in user_keys else None
    
    cursor.execute(f'''
    INSERT INTO {table} (user_id, user_created_at, user_name, user_screen_name,user_location,
        user_url, user_description, user_protected, user_verified, user_geo_enabled,
        user_lang, user_followers_count, user_friends_count, user_favourites_count,
        user_statuses_count, user_observed_ts,data_source) VALUES (%s, %s, %s, %s, %s ,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
        ''' ,
           (user_id, user_created_at, user_name, user_screen_name, user_location,
             user_url, user_description, user_protected, user_verified,
             user_geo_enabled,  user_lang, user_followers_count,
             user_friends_count,  user_favourites_count,  user_statuses_count,
             self.timestamp,data_source
           )
    )
              
  def save_followship(self,table = "followships"):
    pass
      
  def save_mentions(self,entities,cursor,table = "mentions",data_source = "streamer"):
    if len(entities["user_mentions"]) != 0:
      for m in entities["user_mentions"]:
        cursor.execute(f'''
        INSERT INTO {table} (tweet_id, mentioned_user_id, mentioned_screen_name,data_source) VALUES (%s, %s, %s,%s)''',
       (self.tweet_id, m['id'], m['screen_name'],data_source)
    )

  
  def save_hashtags(self,entities,cursor,table = "hashtags",data_source = "streamer"):
    if len(entities["hashtags"]) != 0:
      for h in entities["hashtags"]:
          cursor.execute(f'''
          INSERT INTO {table} (tweet_id, hashtag,data_source) VALUES (%s, %s,%s)''',
         (self.tweet_id, h['text'],data_source)
    )

      
  def save_urls(self,entities,cursor,table = "urls",data_source = "streamer"):
    if len(entities["urls"]) != 0:
      for u in entities["urls"]:
        cursor.execute(f'''
        INSERT INTO {table} (tweet_id, url, expanded_url,data_source) VALUES (%s, %s, %s,%s)''',
       (self.tweet_id, u['url'],u['expanded_url'],data_source)
    )

  
  def write_tweet(self,conn,data_source,dbtype):
      
      # set up cursor
    cursor = conn.cursor()
    
    if (self.is_quote_status == False) and (self.is_retweeted_status == False): 
    
        # Main user
      self.save_user_static(user = self.user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()

      # save tweet data
      self.save_tweet_static(is_retweeted = False, is_quoted = False,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = False, is_quoted = False,table = "tweets_dynamic",data_source = data_source)
      conn.commit()

      # entities
      self.save_mentions(entities = self.tweet_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.tweet_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.tweet_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()
      
      # if it is quoted tweet
    if (self.is_quote_status == True) and (self.is_retweeted_status == False):
        
      # Main user
      self.save_user_static(user = self.user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()
      
      # Quoted user
      self.save_user_static(user = self.quoted_user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.quoted_user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()
      
      # Original/Quoted Tweet
      self.save_tweet_static(is_retweeted = False, is_quoted = False,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = False, is_quoted = False,table = "tweets_dynamic",data_source = data_source)             
      conn.commit()
      
      # Quoting Tweet
      self.save_tweet_static(is_retweeted = False, is_quoted = True,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = False, is_quoted = True,table = "tweets_dynamic",data_source = data_source)             
      conn.commit()
      
      # main entities
      self.save_mentions(entities = self.tweet_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.tweet_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.tweet_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()
      
      # Entities
      self.save_mentions(entities = self.quoted_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.quoted_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.quoted_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()
    
    # if it is retweet
    if (self.is_quote_status == False) and (self.is_retweeted_status == True):

      # Main user
      self.save_user_static(user = self.user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()
      
      # Retweeted user
      self.save_user_static(user = self.retweeted_user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.retweeted_user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()
      
      # Retweeting Tweet
      self.save_tweet_static(is_retweeted = True, is_quoted = False,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = True, is_quoted = False,table = "tweets_dynamic",data_source = data_source)              
      conn.commit()
      
      # Original/Retweeted Tweet
      self.save_tweet_static(is_retweeted = False, is_quoted = False,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = False, is_quoted = False,table = "tweets_dynamic",data_source = data_source)              
      conn.commit()
      
      # main entities
      self.save_mentions(entities = self.tweet_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.tweet_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.tweet_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()            
      
      # entities
      self.save_mentions(entities = self.retweeted_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.retweeted_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.retweeted_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()
        
    if (self.is_quote_status == True) and (self.is_retweeted_status == True):
        
      # Main user
      self.save_user_static(user = self.user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()
      
      # Retweeted user
      self.save_user_static(user = self.retweeted_user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.retweeted_user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()
      
      # Quoted user
      self.save_user_static(user = self.quoted_user,cursor = cursor,table = "users_static",data_source = data_source,dbtype = dbtype)
      self.save_user_dynamic(user = self.quoted_user,cursor = cursor,table = "users_dynamic",data_source = data_source)
      conn.commit()
      
      # Retweeted Tweet
      self.save_tweet_static(is_retweeted = True, is_quoted = False,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = True, is_quoted = False,table = "tweets_dynamic",data_source = data_source)              
      conn.commit()
      
      # Quoted Tweet
      self.save_tweet_static(is_retweeted = False, is_quoted = True,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = False, is_quoted = True,table = "tweets_dynamic",data_source = data_source)              
      conn.commit()
      
      # Main Tweet
      self.save_tweet_static(is_retweeted = False, is_quoted = False,table = "tweets_static",data_source = data_source,dbtype = dbtype)
      self.save_tweet_dynamic(is_retweeted = False, is_quoted = False,table = "tweets_dynamic",data_source = data_source)              
      conn.commit()
      
      # main entities
      self.save_mentions(entities = self.tweet_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.tweet_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.tweet_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()
      
      # quoted entities
      self.save_mentions(entities = self.quoted_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.quoted_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.quoted_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()
      
      # retweeted entities
      self.save_mentions(entities = self.retweeted_entities,cursor = cursor,table = "mentions",data_source = data_source)
      self.save_urls(entities = self.retweeted_entities,cursor = cursor,table = "urls",data_source = data_source)
      self.save_hashtags(entities = self.retweeted_entities,cursor = cursor,table = "hashtags",data_source = data_source)
      conn.commit()
      

cursor = connection.cursor()


print(in_files)

for q,f in enumerate(in_files):
  print(f)
  print(q)
  print(str(datetime.datetime.now()))
  with open(f, 'rb') as instream:
    for i,line in enumerate(instream):
        if i % 25000 == 0: print(i)
        try:
            x = json.loads(line)
            if (list(x.keys())[0] != "delete") and (list(x.keys())[0] != "limit") \
            and (list(x.keys())[0] != "disconnect") and (list(x.keys())[0] != "user_withheld") and (list(x.keys())[0] != "status_withheld"):
                try:
                    xx = tweet(x)
                    xx.write_tweet(conn = connection,data_source = "china_streamer",dbtype = "postgres")
                except:
                    print("error with tweet " + str(i))
        except:
            print("outer error with tweet " + str(i))

connection.close()

