import os
import openai
import aiolimiter

from aiohttp import ClientSession
import asyncio
from typing import Any, List, Dict, Union
from tqdm.asyncio import tqdm_asyncio
import time


async def _throttled_openai_chat_completion_acreate(
    client: openai.AsyncOpenAI,   
    model: str,    
    messages: List[Dict[str, str]],    
    temperature: float,    
    max_tokens: int,    
    top_p: float,    
    stop: Union[str, List[str]],    
    limiter: aiolimiter.AsyncLimiter, ) -> Dict[str, Any]:      
    
    async with limiter:
        trial = 0
        while trial < 5:
            try:                
                return await client.chat.completions.create(                    
                    model=model,                    
                    messages=messages,                    
                    temperature=temperature,                    
                    max_tokens=max_tokens,                    
                    top_p=top_p
                    # response_format={"type": "json_object"}              
                )
            except Exception as e:
                print(e)
                await asyncio.sleep(30*(trial+2))
            trial += 1
        return {"choices": [{"message": {"content": ""}}]}  


async def generate_from_openai_chat_completion(
    messages_list: List[Dict[str, str]],
    model: str,
    temperature: float,
    max_tokens: int,
    top_p: float,
    stop: Union[str, List[str]],
    requests_per_minute: int = 300,
) -> List[str]:
    if model.startswith("gpt-4"):
        requests_per_minute = 200
    if "OPENAI_API_KEY" not in os.environ:
        raise ValueError(
            "OPENAI_API_KEY environment variable must be set when using OpenAI API."
        )
    openai.api_key = os.environ["OPENAI_API_KEY"]
    limiter = aiolimiter.AsyncLimiter(requests_per_minute)
    client = openai.AsyncOpenAI()
    async_responses = [
        _throttled_openai_chat_completion_acreate(
            client=client,
            model=model,
            messages=messages,
            temperature=temperature,
            max_tokens=max_tokens,
            top_p=top_p,
            stop=stop,
            limiter=limiter,
        )
        for messages in messages_list
    ]
    responses = await tqdm_asyncio.gather(*async_responses)

    return responses

from openai import OpenAI


def _throttled_openai_chat_completion_create_sync(   
    model: str,    
    messages: List[Dict[str, str]],    
    temperature: float,    
    max_tokens: int,    
    top_p: float,    
    stop: Union[str, List[str]] ) -> Dict[str, Any]:      
        trial = 0
        while trial < 5:
            try:
                client = OpenAI()                
                return client.chat.completions.create(                    
                    model=model,                    
                    messages=messages,                    
                    temperature=temperature,                    
                    max_tokens=max_tokens,                    
                    top_p=top_p,                    
                    stop=stop,                
                )            
            except Exception as e:
                print(e)
                time.sleep(30*(trial+2))
            trial += 1
        return {"choices": [{"message": {"content": ""}}]}  


def generate_from_openai_chat_completion_sync(
    messages_list: List[Dict[str, str]],
    model: str,
    temperature: float,
    max_tokens: int,
    top_p: float,
    stop: Union[str, List[str]],
    requests_per_minute: int = 300,
) -> List[str]:
    if model == "gpt-4":
        requests_per_minute = 200
    if "OPENAI_API_KEY" not in os.environ:
        raise ValueError(
            "OPENAI_API_KEY environment variable must be set when using OpenAI API."
        )
    openai.api_key = os.environ["OPENAI_API_KEY"]

    sync_responses = [
        _throttled_openai_chat_completion_create_sync(
            model=model,
            messages=messages,
            temperature=temperature,
            max_tokens=max_tokens,
            top_p=top_p,
            stop=stop
        )
        for messages in messages_list
    ]
    responses = sync_responses

    return responses

def gpt(prompt, model="gpt-4", temperature=0.7, max_tokens=1000, n=1, stop=None) -> list:
    return gpts([prompt] * n, model=model, temperature=temperature, max_tokens=max_tokens, stop=stop)[0]

def gpts(prompts, model="gpt-4", temperature=0.7, max_tokens=1000, stop=None) -> list:
    messages_list = [[{"role": "user", "content": prompt}] for prompt in prompts]
    return chatgpts(messages_list, model=model, temperature=temperature, max_tokens=max_tokens, stop=stop)



def chatgpt(messages, model="gpt-4", temperature=0.7, max_tokens=1000, n=1, stop=None) -> list:
    return chatgpts([messages] * n, model=model, temperature=temperature, max_tokens=max_tokens, stop=stop)[0]

def chatgpts(messages_list, model="gpt-4", temperature=0.7, max_tokens=1000, stop=None, max_messages=200) -> list:
    texts = []
    time_start = time.time()
    for i in range(0, len(messages_list), max_messages):
        responses =  asyncio.run(generate_from_openai_chat_completion(model=model, messages_list=messages_list[i: i + max_messages], temperature=temperature, max_tokens=max_tokens, top_p=1, stop=stop))
        texts.extend([x.choices[0].message.content for x in responses])

    return texts

def gpts_sync(prompts, model="gpt-4", temperature=0.7, max_tokens=1000, stop=None) -> list:
    messages_list = [[{"role": "user", "content": prompt}] for prompt in prompts]
    return chatgpts_sync(messages_list, model=model, temperature=temperature, max_tokens=max_tokens, stop=stop)


def chatgpts_sync(messages_list, model="gpt-4", temperature=0.7, max_tokens=1000, stop=None, max_messages=200) -> list:
    texts = []
    for i in range(0, len(messages_list), max_messages):
        responses =  generate_from_openai_chat_completion_sync(model=model, messages_list=messages_list[i: i + max_messages], temperature=temperature, max_tokens=max_tokens, top_p=1, stop=stop)
        texts.extend([x.choices[0].message.content for x in responses])

    return texts
