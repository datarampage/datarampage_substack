import os
import pandas as pd
import shutil

def dupe_finder(directory, stringval):
    
    import glob

    os.chdir(directory)
    
    file_df = pd.DataFrame()
    
    #check if there are sub-directories
    
    x = set([os.path.dirname(p) for p in glob.glob(directory+'/*/*')])
	
	#one operation for sub-directories

    if len(x) > 0:
        
        folders = os.listdir(directory)
    
        if 'Thumbs.db' in folders: folders.remove('Thumbs.db')
  
        for i in range(len(folders)):
            new_path = directory + '/' + folders[i]
            os.chdir(new_path)
        
            temp_files = os.listdir(new_path)
            temp_df = pd.DataFrame({'files': temp_files,'duplicate': [stringval in f for f in temp_files]})
            temp_df['file_path'] = new_path + '/' + temp_df['files']
            temp_df = temp_df[temp_df['files'].str.endswith('.mp3')]
            file_df = pd.concat([file_df,temp_df])
    
	#another operation for direct folders	
    else:
        
        temp_files = os.listdir(directory)
        temp_df = pd.DataFrame({'files': temp_files,'duplicate': [stringval in f for f in temp_files]})
        temp_df['file_path'] = directory + '/' + temp_df['files']
        temp_df = temp_df[temp_df['files'].str.endswith('.mp3')]
        file_df = pd.concat([file_df,temp_df])

    return file_df

def dupe_remover(df):
    files_to_remove = df[df['duplicate'] == True]['file_path'].tolist()
    for file in files_to_remove:
        os.remove(file)
    print('All files removed')
	
def dupe_mover(df, new_path):
    files_to_move = df[df['duplicate'] == True]['file_path'].tolist()
    for file in files_to_move:
        shutil.move(file, new_path)
    print('All files moved to ' + new_path)
	

rock = dupe_finder('H:/My Music/rock',' (1).mp3')

dupe_mover(rock)

