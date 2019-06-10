import requests
from bs4 import BeautifulSoup
import re
import time
import json

# The following method catched all the PTT Food pages' URL from year 2004 to today
urls = ['https://www.ptt.cc/bbs/Food/index{}.html'.format(str(i)) for i in range(1,7001)]
urls = urls[::-1]
url_list = urls[250:300] # <-- Change the range to assign the pages to crawl

start = time.time()
print('PTT program starts...')
# Get all the articles' URL in the given page
content_list = []
comments_list = []
except_list = []

page = len(url_list)
for url in url_list:
    print('The', page, 'page', url,'starts:')
    #page -= 1
    res = requests.get(url, verify=False)
    time.sleep(2)
    html_str = res.text
    soup = BeautifulSoup(html_str)

    title_list = []

    for i in soup.select('.title a'):
        route = 'https://www.ptt.cc' + i.get('href')
        title_list.append(route)
    # print(title_list)
    num = len(title_list)
    # Finally, we are able to get the contents
    for url2 in title_list:
        print('The page',page,num, url2)
        num -= 1
        time.sleep(2)
        r2 = requests.get(url2)
        soup = BeautifulSoup(r2.text, 'html')
        main_content = soup.select('#main-content')

        info = soup.select('.article-metaline')
        # The main content is the whole thing except the article's info
        info = [i.text for i in info]
        # Get the title
        if info != []:
            try:
                title = info[1]
                # Get the time
                date = info[2].replace('時間', '')
                info.insert(1, '看板Food')
                info_str = ''.join(info)
                # Clean the title, author, and other info
                main_content = main_content[0].text.replace(info_str, '')

                # Split the whole thing into the main content and the comments sections
                main_content = main_content.split('※ 發信站: 批踢踢實業坊')
                content = main_content[0]  # This is the main content
                # Start clean the content
                # Clean out '\n'
                content = content.replace('\n', '')
                # Clean all the links
                content = re.sub(
                    '(?:(?:https?|ftp|file):\/\/|www\.|ftp\.)(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[-A-Z0-9+&@#\/%=~_|$?!:,.])*(?:\([-A-Z0-9+&@#\/%=~_|$?!:,.]*\)|[A-Z0-9+&@#\/%=~_|$])',
                    '', content)
                if len(main_content) >= 2:
                    
                    comments = main_content[1]  # This section is all the comments
                    comments = comments.split('.html')
                    # Get the IP address
                    ip_addr = comments[0].split('※ 文章網址')
                    ip_addr = ip_addr[0].split('來自: ')
                    ip_addr = ip_addr[1]
                    ip_addr = ip_addr.replace('\n', '')
                    comments = comments[1]
                    comments = comments.split('\n')
                    # Count pushes and boos, and append all comments
                    push = 0
                    boo = 0
                    comment_count = 0
                    all_comments = ''
                    for i in comments:
                        if len(i) != 0:
                            if i[0] == '推':
                                push += 1
                            elif i[0] == '噓':
                                boo += 1
                            elif i[0] == '→':
                                comment_count += 1
                            i = i.split(' ')
                            if i[0] != '※':
                                i = i[2:-2]
                                i = ''.join(i)
                                all_comments += i

                    total_comment_count = comment_count + push + boo

                    content_list.append(
                        {'URL': url2, 'Title': title, 'Time': date, 'IP': ip_addr, 'Content': content, '推文': push,
                         '噓文': boo, '總回文數': total_comment_count})
                    comments_list.append({'URL': url2, 'Comment': all_comments})
            except:
                except_list.append(url2)    
        else:
            except_list.append(url2)
    page -= 1
with open('250_300_content.json', 'w', encoding='utf-8') as file:
    json.dump(content_list, file, ensure_ascii= False)
with open('250_300_comments.json', 'w', encoding='utf-8') as file:
    json.dump(comments_list, file, ensure_ascii= False)
with open('250_300_log.json', 'w', encoding='utf-8') as file:
    json.dump(except_list, file, ensure_ascii= False)
end = time.time()
minute = round((end - start) / 60)
second = round((end - start) % 60)
print('Finished')
print('Total time:', minute, 'm', second, 's')