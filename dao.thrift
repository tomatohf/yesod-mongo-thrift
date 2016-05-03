struct AccessResult {
    1: optional list<i32> data
    2: list<string> message
}

service Dao {
    map<string, string> getSession(1: string cookie, 2: list<string> keys);
    AccessResult getAccountAccess(1: string id)
}
