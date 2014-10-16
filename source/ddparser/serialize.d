module ddparser.serialize;

import std.traits;
import std.json;
import std.string;
import core.stdc.string;
import std.stdio;

template fieldMap(string field, alias s)
{
    enum fieldMap = `
        if ( ` ~ s.stringof ~ `.serializing)
        {
            ` ~ s.stringof ~ `.currentObject["` ~ field ~ `"] = ` ~ s.stringof ~ `.serializeValue(` ~ field ~ `);
        }
        else
        {
            typeof(` ~ field ~ `) f;
            ` ~ s.stringof ~ `.deserializeValue(f, ` ~ s.stringof ~ `.currentObject["` ~ field ~ `"]);
            ` ~ field ~ ` = f;
        }
    `;
}

class Serializer
{
    JSONValue serialize(T)(ref T v)
    {
        objects = null;
        indexes = null;
        currentObject = null;

        serializing = true;
        JSONValue rootObject = serializeValue(v);
        return JSONValue(["root" : rootObject, "objects" : JSONValue(objects)]);
    }

    void deserialize(T)(ref T v, in JSONValue val)
    {
        currentObject = null;

        serializing = false;
        objects = val["objects"].array.dup;
        pointers = new void*[objects.length];
        deserializeValue(v, val["root"]);
    }


    void map(T)(ref T v, string name)
    {
        if (serializing)
        {
            currentObject[name] = serializeValue(v);
        }
        else
        {
            try
            {
            deserializeValue(v, currentObject[name]);
            }
            catch(Exception e)
            {
                writeln("Could not deserialize: ", name);
                throw e;
            }

        }
    }

    void mapCString(ref char* v, string name)
    {
        if (serializing)
        {
            currentObject[name] = serializeCString(v);
        }
        else
        {
            v = deserializeCString(currentObject[name]);
        }
    }

    void mapCArray(T, L)(ref T* v, string arrName, ref L len)
    {
        if (serializing)
        {
            JSONValue[] arr;
            foreach(i; v[0 .. len])
            {
                arr ~= serializeValue(i);
            }

            currentObject[arrName] = JSONValue(arr);
        }
        else
        {
            auto desArr = currentObject[arrName].array;
            len = cast(L)desArr.length;
            T[] arr = new T[len];
            for (int i = 0; i < len; ++i)
            {
                deserializeValue(arr[i], desArr[i]);
            }
            v = arr.ptr;
        }
    }

    void mapCStringWithLength(L)(ref char* v, string name, ref L len)
    {
        if (serializing)
        {
            currentObject[name] = JSONValue(cast(string)v[0 .. len]);
        }
        else
        {
            string val = currentObject[name].str;
            v = cast(char*)val.dup.toStringz();
            len = cast(L)val.length;
        }
    }

    JSONValue serializeCString(const char* v)
    {
        if (!v) return JSONValue(null);
        uint* index = v in indexes;
        if (index) return JSONValue(*index);
        objects ~= JSONValue(cast(string)v[0 .. strlen(v)]);
        uint result = cast(uint)objects.length - 1;
        indexes[v] = result;
        return JSONValue(result);
    }

    char* deserializeCString(JSONValue j)
    {
        if (j.type == JSON_TYPE.NULL) return null;
        auto index = j.uinteger;
        char* result = cast(char*)pointers[index];
        if (!result)
        {
            result = cast(char*)objects[index].str.toStringz();
            pointers[index] = result;
        }
        return result;
    }

    JSONValue serializePointer(T)(ref T v)
    {
        if (v == null) return JSONValue(null);
        uint* index = v in indexes;
        if (index) return JSONValue(*index);
        uint result = cast(uint)objects.length;
        indexes[v] = result;
        objects ~= JSONValue(null);
        objects[result] = serializeValue(*v);
        return JSONValue(result);
    }

    void deserializePointer(T)(ref T v, JSONValue j)
    {
        if (j.type == JSON_TYPE.NULL)
        {
            v = null;
            return;
        }
        auto index = j.uinteger;
        v = cast(T)pointers[index];
        if (!v)
        {
            v = new typeof(*v)();
            pointers[index] = v;
            deserializeValue(*v, objects[index]);
        }
    }

    JSONValue serializeValue(T)(in T v) if (isNumeric!T || isBoolean!T)
    {
        return JSONValue(v);
    }

    JSONValue serializeValue(T)(ref T v) if (!isNumeric!T && !isBoolean!T)
    {
        static if (isPointer!T)
        {
            static if (is(Unqual!T == char*))
            {
                return serializeCString(v);
            }
            else static if (is(Unqual!T == void*))
            {
                assert(false);
            }
            else
            {
                return serializePointer(v);
            }
        }
        else static if (is(T == struct))
        {
            static if (__traits(hasMember, T, "customSerialize"))
            {
                return v.customSerialize(this);
            }
            else static if (__traits(hasMember, T, "serialize"))
            {
                auto oldObject = currentObject;
                currentObject = null;
                v.serialize(this);
                auto res = currentObject;
                currentObject = oldObject;
                return JSONValue(res);
            }
            else
            {
                return JSONValue("ERROR!!!");
            }
        }
        else static if (isArray!T)
        {
            JSONValue[] arr;
            foreach(i; v)
            {
                arr ~= serializeValue(i);
            }
            return JSONValue(arr);
        }
    }

    void deserializeValue(T)(ref T v, JSONValue j)
    {
        static if (isIntegral!T)
        {
            static if (isSigned!T)
                v = cast(T)j.integer;
            else
                v = cast(T)j.uinteger;
        }
        else static if (isBoolean!T)
        {
            v = cast(bool)j.uinteger;
        }
        else static if (isFloatingPoint!T)
        {
            v = cast(T)j.floating;
        }
        else static if (isPointer!T)
        {
            static if (is(Unqual!T == char*))
            {
                v = deserializeCString(j);
            }
            else static if (is(Unqual!T == void*))
            {
                assert(false);
            }
            else
            {
                deserializePointer(v, j);
            }
        }
        else static if (is(T == struct))
        {
            static if (__traits(hasMember, T, "customDeserialize"))
            {
                v.customDeserialize(this, j);
            }
            else static if (__traits(hasMember, T, "serialize"))
            {
                auto oldObj = currentObject;
                currentObject = j.object;
                v.serialize(this);
                currentObject = oldObj;
            }
            else
            {
                
            }
        }
        else static if (isArray!T)
        {

        }
    }

    bool serializing;
    JSONValue[] objects;
    uint[void*] indexes;
    void*[] pointers;
    JSONValue[string] currentObject;
}


