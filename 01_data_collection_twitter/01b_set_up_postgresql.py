import psycopg2

# First: Create Database:

dbname = "xxx"
con = psycopg2.connect(user = 'xxxxx', 
  password = 'xxxxxx',
  database = dbname, 
  host = 'xxxxxx', 
  port = "xxxxxx")

cursor = con.cursor()

###
cursor.execute("""SELECT table_name FROM information_schema.tables
  WHERE table_schema = 'public'""")

for table in cursor.fetchall():
  print(table)
        

##
cursor.execute('''
CREATE TABLE users_static(
    user_id bigint PRIMARY KEY,
    user_id_str text,
    data_source text
);
''')

##
cursor.execute('''
CREATE TABLE users_dynamic(
    user_id bigint REFERENCES users_static(user_id), 
    user_created_at TIMESTAMP NULL,
    user_name text, 
    user_screen_name text,
    user_location text,
    user_url text, 
    user_description text, 
    user_protected BOOLEAN,
    user_verified BOOLEAN, 
    user_geo_enabled BOOLEAN,
    user_lang TEXT, 
    user_followers_count INTEGER, 
    user_friends_count INTEGER, 
    user_favourites_count INTEGER,
    user_statuses_count INTEGER,
    user_observed_ts TIMESTAMP NULL,
    data_source text
)
;
''')

#commit
con.commit()


# Tweets (not retweets/quoted tweets) - Static parts

cursor.execute('''
CREATE TABLE tweets_static(
    tweet_id BIGINT PRIMARY KEY,
    tweet_id_str TEXT,
    full_text TEXT,
    source TEXT NULL,
    tweet_created_at TIMESTAMP NULL,
    user_id BIGINT REFERENCES users_static(user_id),
    is_retweeted BOOLEAN,
    is_quoted BOOLEAN,
    is_retweeting BOOLEAN,
    is_quoting BOOLEAN,
    in_reply_to_status_id BIGINT,
    in_reply_to_user_id BIGINT,
    in_reply_to_screen_name TEXT,
    retweet_id BIGINT,
    quoted_id BIGINT,
    data_source text
);
''')

#Tweets - dynamic content

cursor.execute('''
CREATE TABLE tweets_dynamic(
    tweet_dynamic_id serial PRIMARY KEY,
    tweet_id BIGINT REFERENCES tweets_static(tweet_id),
    quote_count INTEGER,
    reply_count INTEGER,
    retweet_count INTEGER,
    favorite_count INTEGER,
    timestamp_observed TIMESTAMP NULL,
    data_source text
);
''')

con.commit()

#Mentions

cursor.execute('''
CREATE TABLE mentions(
    mention_id serial PRIMARY KEY,
    tweet_id BIGINT REFERENCES tweets_static(tweet_id),
    mentioned_user_id BIGINT,
    mentioned_screen_name TEXT,
    data_source text
);
''')

cursor.execute('''
CREATE TABLE urls(
    url_id serial PRIMARY KEY,
    tweet_id BIGINT REFERENCES tweets_static(tweet_id),
    url TEXT,
    expanded_url TEXT,
    data_source text
);
''')

cursor.execute('''
CREATE TABLE hashtags(
    hashtag_id serial PRIMARY KEY,
    tweet_id BIGINT REFERENCES tweets_static(tweet_id),
    hashtag TEXT,
    data_source text
);
''')

cursor.execute('''
CREATE TABLE followerships(
    followership_id serial PRIMARY KEY,
    user_id_from BIGINT,
    user_id_to BIGINT,
    timestamp_observed TIMESTAMP NULL,
    type_observed TEXT,
    data_source text
);
''')

con.commit()

con.close()
print("done")