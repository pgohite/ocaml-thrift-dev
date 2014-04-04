typedef i16 session_key
typedef i32 session_id


enum Numberz
{
  ONE = 1,
  TWO,
  THREE,
  FIVE = 5,
  SIX,
  EIGHT = 8,
}

struct stname {
    1: string fname,
    2: string lname,
    3: i16  age,
    4: bool   m_or_f,
}

struct address {
    1: sname  nname 
    2: string street
    3: i32  zipcode
}

service TestService {
  oneway void  testVoid(),
  void testTowWayVoid (),
  bool testTowWayVoid (),
  address testCustomReturn (1: i32 int32_arg, 2: string username, 3: Numberz nums),
  oneway address testCustomReturn2 (1: i32 int32_arg, 2: string username)
}
