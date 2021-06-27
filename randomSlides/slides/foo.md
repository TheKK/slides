---
paginate: true
---

# 有效率地跟老扣一起工作

Ying-Ruei Linag - SIT

---

# 先來個簡單的分析

---

<!--
header: 案例分析
-->

## 案例

給你看一段程式碼

```rust
// 忽略
let 回傳值 = 使用資源(資源)
// 忽略
```

請問，這裡的 `使用資源` 跟 `資源` 可能從哪裡來的呢？

---

## 可能 A

來自全域變數

```rust
// 全域定義
let 使用資源 = ooo()
let 資源 = OOO()

// 使用起來像是這樣

fn 方程式_甲() -> XXX {
  // 忽略
  let 回傳值 = 使用資源(資源)
  // 忽略
}
```

---

## 可能 B

來自本地變數

```rust
fn 方程式_甲() -> XXX {
  // 本地定義
  let 使用資源 = ooo()
  let 資源 = OOO()

  // 使用起來像是這樣

  // 忽略
  let 回傳值 = 使用資源(資源)
  // 忽略
}
```

---

## 可能 C

來自參數

```rust
// 參數傳遞
fn 方程式_甲(使用資源, 資源) -> XXX {
  // 使用起來像是這樣

  // 忽略
  let 回傳值 = 使用資源(資源)
  // 忽略
}
```

> 2020 年，我想函數可以像數值一樣被傳遞，應該是個容易被接受的想法了。
> 函數就是一種數值，可以被傳遞，可以被修改，可以被執行。

---

## 嗯？怎麽好像差異很小？

是的，我們的

```rust
// 忽略
let 回傳值 = 使用資源(資源)
// 忽略
```

從來沒變過，只是被放的位置不同而已

---

## 差異

全域變數，本地變數跟從參數傳遞，他們有個明確的差異：

**替換掉的難度**

這裡的替換，不僅限於為測試所做的替換。為了增加使用的範圍，降低使用難度等，有許多原因會希望程式中的某些行為是可以被替換的。

---

## 手段

| 替換時間/類型 | 全域變數                               | 本地變數 | 參數傳遞                   |
|---------------|----------------------------------------|----------|----------------------------|
| 編譯時期      | 編譯時連結不同的實作，C macro 類的魔法 | *不好說  | **靜態**選擇要傳遞什麼參數 |
| 執行時期      | 修改全域變數的值                       | *不好說  | **動態**選擇要傳遞什麼參數 |

> *不好說：對本地變數來說，替換等號右側的描述式的問題，可以依照涉及變數是全域/本地/參數傳遞，遞迴討論下去。

---

## 難度

| 替換時間/類型 | 全域變數           | 本地變數 | 參數傳遞       |
|---------------|--------------------|----------|----------------|
| 編譯時期      | 高，涉及編譯細節   | 不好說       | 低，傳參數而已 |
| 執行時期      | 中，需注意影響範圍跟巢狀問題 | 不好說       | 低，傳參數而已 |

## 影響範圍

| 替換時間/類型 | 全域變數     | 本地變數           | 參數傳遞       |
|---------------|--------------|--------------------|----------------|
| 編譯時期      | 所有用到的人 | 本地區域內的所有人 | 函數內的所有人 |
| 執行時期      | 所有用到的人 | 本地區域內的所有人 | 函數內的所有人 |

---

## 共識時間

- 全域變數的東西很難修改
  - 每個用戶的選擇，都 **會** 影響到彼此，交互運作
- 本地變數可能由全域變數或函數參數組成，難度看組合而定
- 參數傳遞最容易修改
  - 每個用戶的選擇，都 **不會** 影響到彼此，獨立運作

---

## 有趣的觀察

其實，所有東西都可以寫成 `參數傳遞` 的形式

我們定義下列函數

```rust
fn 方程式_甲_參數型(使用資源, 資源) -> XXX {
  // 忽略
  let 回傳值 = 使用資源(資源)
  // 忽略
}
```

---

## 全域的，可以被改寫

```rust
let 全域_使用資源 = ooo()
let 全域_資源 = OOO()

fn 方程式_甲() -> XXX {
  // 忽略
  let 回傳值 = 全域_使用資源(全域_資源)
  // 忽略
}

// 可以寫成 ==>

fn 方程式_甲() -> XXX {
  return 方程式_甲_參數型(全域_使用資源, 全域_資源)
}
```

---

## 本地的，可以被改寫

```rust
fn 方程式_甲() -> XXX {
  let 本地_使用資源 = ooo()
  let 本地_資源 = OOO()

  // 忽略
  let 回傳值 = 本地_使用資源(本地_資源)
  // 忽略
}

// 可以寫成 ==>

fn 方程式_甲() -> XXX {
  let 本地_使用資源 = ooo()
  let 本地_資源 = OOO()

  return 方程式_甲_參數型(本地_使用資源, 本地_資源)
}
```

---

## 共識時間

- 無論什麼形式的實作，通通都可以等價被抽換成 `參數傳遞` 型的呼叫
  - 接下來的投影片，我們叫他 `簡單函數抽取` 方面相互參考
- 儘可能在一開始撰寫程式碼時就偏好 `參數傳遞` 型的實作方式，因為他的 **彈性** 和 **適應能力** 最強

## 哲學時間

- 如果所有長度的程式碼都切割一半，分別由兩個新產生的函數來取代，那麼理論上我們可以寫出所有函數內都只有兩行程式碼的東西
  - 那程式碼的長度和理解難易度，還是正相關嗎？(想想我們前端的 callback，分開/簡短的程式碼，是否總是更容易理解)

---

## 預告時間

等等我們會不停看到 `簡單函數抽取` 的出現，搭配一些 **物件導向特性** 一起組合出書中的修改方法

---

<!--
header: ""
-->

# 準備完畢，那我們就可以開始了！

但是我們稍微調整一下順序，先介紹最重要的

---

<!--
header: 25.15 參數化方法
-->

# 25.15 參數化方法

---

## 原本在 run 裡直接建構

```cpp
void TestCase::run() {
    delete m_result;
    m_result = new TestResult(); // <- 想換掉這個的話
    
    try {
        setUp();
        runTest(m_result);

    } catch(exception& e) {
        result->addFailure(e, this); 
    }

    tearDown();
}
```

目標：讓外部可以替換掉 TestResult 的建構方式

---

## 可以改成這樣，交給呼叫端決定內容

```cpp
void TestCase::run(TestResult* result) { // <- 從這裡來
    delete m_result;
    m_result = result; // <- 在這裡用
    
    try {
        setUp();
        runTest(m_result);

    } catch(exception& e) {
        result->addFailure(e, this); 
    }

    tearDown();
}
```

`簡單函數抽取`！

---

## 看似直覺，卻充滿陷阱

書上的修改其實是有問題的，你有看出來嗎

---

## 陷阱

```cpp
void TestCase::run() {
    delete m_result;
    m_result = new TestResult();
    
// 執行順序
// (1) 刪除舊的 m_result，呼叫他的解構子
// (2) 呼叫 TestResult()
// (3) 更新 m_result 的數值
    
void TestCase::run(TestResult* result) {
    delete m_result;
    m_result = result;

// 執行順序
// (1) 呼叫 TestResult()，然後傳入 run 內
// (2) 刪除舊的 m_result，呼叫他的解構子
// (3) 更新 m_result 的數值
```

---

## 陷阱解析

執行順序改變，就是改變程式行為，就有機會產生不預期的結果。我們無法保證 `m_result` 的解構子和 `TestResult` 的建構子內是否有循序上的相依。

很遺憾，在無法區分操作是否含有副作用的語言內，所有運算都應該被視為 **含有副作用** 來處理。文言點說就是，這些程式碼都無法保證全是 **引用透明（Referential transparency）** 的

---

## 引用透明（Referential transparency）

> An expression is called referentially transparent if it can be replaced with its corresponding value (and vice-versa) without changing the program's behavior. 
>
> [wikipedia](https://en.wikipedia.org/wiki/Referential_transparency)

## 白話文

如果下列敘述成立，那這段程式碼就是引用透明的

```cpp
let result = f() + f();

// 等價

let f_ret = f();
let result = f_ret + f_ret;
```

---

## 引用透明的好處

```cpp
let result = f() + f();

// 等價

let f_ret = f();
let result = f_ret + f_ret;
```

- 程式碼可以被 cache
- 程式碼可以隨意抽換成任意函數，且不改變行為
- 數個引用透明的程式片段，可以無視順序去計算（容易平行化）
- 沒有執行順序問題，程式設計師可以更容易理解/改善程式碼

---

## 沒有引用透明帶來的問題

- 看似簡單的函數抽取，都有改變行為的風險
  - 特別是 C++，函數 call by ref 跟 by value 在呼叫端的語法是 **沒有區別** 的
    ```cpp
    f(x) // 天知道 x 是被當作參考傳入 f，還是被複製了一份
    ```
    - 因為不能保證複製運算內沒有副作用會產生，因此我們需要區分傳參考和複製，才能理解程式碼真正的行為（效能變化也是一種行為改變）
    - 無法區分，表示 `f` 的上游修改函數簽名時，程式碼的行為雖然改變，編譯器卻照樣編譯，程式設計師無法簡單察覺變動
- 必須預設所有操作都帶有副作用，理解程式碼時無法快速聚焦
  - 因為所有程式碼都可能發生你在意的副作用
    - 很多人都有在名為 getXXX 的函數裡發現過驚喜
- 需要更多的測試，來確保程式行為不被內部/外部的相依影響

---

<!--
將建立 TestResult 的時機點交給 run 去決定
-->

## 回到一開始的陷阱



```cpp
delete m_result;
let test_result = new TestResult();
m_result = result;

// 不等價於

let test_result = new TestResult();
delete m_result;
m_result = test_result;
```

正確的修改方式為 **抽出建立 TestResult 的函數**，此時可以維持原來的執行順序

```cpp
void TestCase::run(fn<() -> TestResult*> mkResult) {
    delete m_result;
    m_result = mkResult();
```

---

## 維持既有界面的修改方式

```cpp
void TestCase::run() {
    return this->runWithMkResult(TestResult);
}

void TestCase::runWithMkResult(fn<() -> TestResult*> mkResult) {
    delete m_result;
    m_result = mkResult();
    // ...
}
```

這就是 `簡單函數抽取` 所帶來的彈性

---

<!--
header: 25.14 參數化建構子
-->

# 25.14 參數化建構子

---

## KISS - keep it simple, stupid

我們不要讓事情複雜

方法是函數，建構式也是函數，因此此小節的技巧跟 25.15 是重複的，都是 `簡單函數抽取` 的應用

---

## 抽取前

```rust
fn mkMailChecker() -> MailChecker {
  let receiver = new MailReceiver();
  // ...
}
```

## 抽取後

```rust
fn mkMailChecker() -> MailChecker {
  return mkMailCheckerWithReceiver(new MailReceiver());
}

fn mkMailCheckerWithReceiver(MailReceiver receiver) -> MailChecker {
  // ...
}
```

---

## 共識時間

- T 的建構式，就是任何回傳 T 的函數
  - fn f() -> T
  - fn f(A, B, C) -> T
- T 的方法，就是任何參數內含有至少一個 T 的函數
  - fn f(T) -> ()
  - fn f(T, A) -> B
  - fn f(T, T) -> Bool
- 要說差異的話，在於 **類別欄位的可視範圍**

---

<!--
header: 25.5 暴露靜態方法
-->

# 25.5 暴露靜態方法

接著，我們就按照順序來

---

## 我們用 Python like 的方式來描述問題（注意 self）

```java
class RSCWorkflow {
  ...
  public void validate(self, Packet packet) {
    // 重點：validate 裡都沒用到 self
    if (packet.getOriginator() == "MIA" ||
        packet.getLength() > MAX_LENGTH ||
        !packet.hasValidCheckSum()) {
        throw new InvalidFlowException();
    }
    ...
  }
  ...
}
```

目標：測試這段被鑲嵌在方法內的可憐 **純函數**

---

## 沒用到的參數該怎麼辦？

嗯，移掉就好

---

## 移掉沒用到的參數，且維持既有界面

```java
void validatePacket(Packet packet) {
  if (packet.getOriginator() == "MIA" ||
      packet.getLength() > MAX_LENGTH ||
      !packet.hasValidCheckSum()) {
      throw new InvalidFlowException();
  }
  ...
}

class RSCWorkflow {
  ...
  public void validate(self, Packet packet) {
    return validatePacket(packet);
  }
  ...
}
```

還是 `簡單函數抽取`

---

## 什麼，你說書上抽的明明是靜態 "方法"

---

## 維持不知道有什麼好處的靜態方法的界面

```java
void validatePacket(Packet packet) {
  ...
}

class RSCWorkflow {
  ...
  public void validate(self, Packet packet) {
    return validatePacket(packet);
  }

  public static void _validatePacket(Packet packet) {
    return validatePacket(packet);
  }
  ...
}
```

自由函數維持不變，輕鬆加入所需要的類別靜態方法

---

## 書中也提到，將 validate 放到 Packet 的可能性

```java
void validatePacket(Packet packet) {
  ...
}

class Packet {
  ...
  public void validate(self) {
    return validatePacket(self);
  }
  ...
}
```

還是很方便

---

## 哲學時間

- 為什麼明明沒有用到 self，我們卻會寫出 `void validate(self, Packet packet)`？
- 為什麼沒有用到 self，編譯器沒有提供警告/建議？
- 彈性最高（可以輕易構成方法和靜態方法）的 free function 形式，為什麼不在本書的建議之中？

---

<!--
header: 25.6 提取並複寫呼
-->

# 25.6 提取並複寫呼叫

---

## 原本的樣子

```java
public class PageLayout {
  ...
  protected void rebindStyles(self) {
    self.style = StyleMaster.fromStyles(self.template, self.id);
    ...
  }
  ...
}
```

目標：讓外部可以抽換 StyleMaster.fromStyles 的實作

---

## 讓外部能夠用繼承的方式來複寫部分邏輯

```java
public class PageLayout {
  ...
  protected void rebindStyles(self) {
    self.style = self.fromStyles(self.template, self.id);
    ...
  }

  // 想要抽換行為的用戶，可以藉由繼承後複寫達成
  protected List fromStyles(self, SystemTemplate template, int id) {
    // 書上的案例，self 沒用到喔
    return StyleMaster.fromStyles(template, id);
  }
  ...
}
```

將原本的寫死的 `StyleMaster.fromStyles` 改為從 `self.fromStyles` 獲取。

---

## 有趣的觀察

```java
public class PageLayout {
  protected void rebindStyles(self) {
    self.style = self.fromStyles(self.template, self.id);
    ...
  }
}
```

單看 `rebindStyles` 的話，`fromStyles` 就像是某種存在 self 裡的變數一般，在某些語言內 `fromStyles` 也真的可以以類別欄位的方式存在。

這讓 **提取為方法** 和 **提取為欄位** 的界線變得模糊，因為欄位一樣可以被複寫（於建構子內），而欄位可以存放函數

我們可以很硬要的說，這就是 `簡單函數抽取` 配合上程式語言特殊功能（複寫）的組合

---

## 有趣的觀察 - II

```java
public class PageLayout {
  ...
  protected List fromStyles(self, SystemTemplate template, int id) {
    return StyleMaster.fromStyles(template, id);
  }
  ...
}
```

書上的範例，`fromStyles` 並非靜態方法，self 是可視的，但 `PageLayout` 內的實作沒用到 self。

這表示，複寫 `fromStyles` 的人將會獲得 **非常高的權限** 。複寫者將可以調用 **所有** 可視的 `PageLayout` 的方法和與其相關的函數，以及修改 `PageLayout` 的可視欄位。

但，移除 self 後 `fromStyles` 就會變成靜態方法，靜態方法又無法被複寫...

---

## 延伸問題

- 複寫後的行為，如何確認與原來的一致？該如何描述方法的行為範圍？
  - 多數複寫機制都無法限縮複寫者的權限，這讓複寫後的行為很容易暴走，甚至破壞類別預期的行為
  - 若有微妙差異，程式行為改變，測試就沒有意義了
- 如何告訴外部哪些方法是為測試而可以去複寫的？
  - 一個待測方法可以涉及複數個帶有副作用的呼叫
- 如何知道複寫的方法影響的範圍有多大？
  - 一個帶有副作用的呼叫，可以被多個方法給使用

---

<!--
header: 25.7 提取並複寫工廠方法
-->

# 25.7 提取並複寫工廠方法

---

## 一開始的程式碼

```java
public class WorkflowEngine {
  public WorkflowEngine(self) {
    let config_a = AppConfig.getDryConfigureation();
    let reader = new ModelReader(config_a);
    
    // 還記得引用透明嗎？看起來一樣也不能輕易取代/互用
    let config_b = AppConfig.getDryConfigureation();
    let presister = new XMLStore(config_b);
    
    self.tm = new TransactionManager(reader, presister);
    ...
  }
}
```

目標：希望能在測試時替換 TransactionManager 的建構方式

---

## 使用我們的好朋友

```java
public class WorkflowEngine {
  public WorkflowEngine(self) {
    self.tm = self.mkTransactionManager();
    ...
  }
  protected TransactionManager mkTransactionManager(self) {
    let config_a = AppConfig.getDryConfigureation();
    let reader = new ModelReader(config_a);
    
    let config_b = AppConfig.getDryConfigureation();
    let presister = new XMLStore(config_b);
    
    return new TransactionManager(reader, presister);
  }
}
```

`簡單函數抽取` 抽離行為，程式語言機制複寫行為

---

## 延伸問題

- 跟 **25.6 提取並複寫呼** 提到的一模一樣
  - 畢竟，建構子跟類別方法沒什麼太大的差異。多數語言在建構子內也都可以摸到 `this`/`self` 了，真的跟一般的方法很接近

---

<!--
header: 25.8 提取並複寫獲取方法
-->

# 25.8 提取並複寫獲取方法

---

## 給 C++

> C++ 裡你無法在基底類別建構子中，呼叫衍生類別的虛擬函式

我知道 C++ 不能這樣做的原因，但我不知道為什麼其他語言可以。同時，沒用到的 `this`/`self` 現在就成為了實作上的困擾了

---

## 稍微簡化問題

```cpp
struct WorkflowEngine {
  WorkflowEngine() {
    this->tm = mkTransactionManager();
    ...
  }

private:
  unique_ptr<TransactionManager> tm;
}
```

目標：希望能在測試時替換 TransactionManager 的建構方式

---

## C++ 能用的方法

```cpp
struct WorkflowEngine {
  WorkflowEngine() { /* 不動 this->tm 了 */ ... }

protected:
  TransactionManager& getTransactionManager() const {
    if (!this->tm) { this->tm = mkTransactionManager(); }
    return *(this->tm);
  }

private:
  unique_ptr<TransactionManager> tm;
}
```

這個方法還需要將所有對於 `this->tm` 的存取，改為 `getTransactionManager` 呼叫，這樣衍生類別繼承 `getTransactionManager` 後才能發揮效果。

另外也要小心，不要在建構子內不小心呼叫到 `getTransactionManager`。

---
    
## 疑惑

| 特性 \ 手段          | 參數傳入       | 繼承複寫       |
|----------------------|----------------|----------------|
| 可以決定要這麼做的人 | 看得到標頭的人 | 看得到標頭的人 |
|                      |                |                |

不知道為什麼不多開一個 `WorkflowEngine(unique_ptr<TransactionManager>)` 的建構子就好了。兩種方法的擴散範圍是一樣的，也都是平常沒事不會不小心用到的手段。

本小節的技巧稍嫌迂迴。

---

<!--
header: 25.9 實作提取
-->

# 25.9 實作提取

---

## 開始

```cpp
struct ModelNode {
  void addNode(ModelNode) { ... }
  void colorize() { ... }

private:
  ...
}
```

目標：將 ModelNode 改為界面的形式，並產生 ProductionModelNode 來盛裝原本 ModelNode 的實作

---

## 結束

```cpp
struct ModelNode {
  virtual ~ModelNode() {}
  void addNode(ModelNode) = 0;
  void colorize() = 0;
}

struct ProductionModelNode : ModelNode {
  void addNode(ModelNode) { ... }
  void colorize() { ... }
  ...
}
```

接著還需要修改程式碼之前建立 ModelNode 的地方，改為 ProductionModelNode，等到專案可以圍城編譯就完成了

---

<!--
header: 25.10 界面提取
-->

# 25.10 實作提取

---

## 開始

```cpp
struct ModelNode {
  void addNode(ModelNode) { ... }
  void colorize() { ... }

private:
  ...
}
```

目標：從 ModelNode 抽出界面的形式，產生 ModelNodeInterface

---

## 跟上一個一模一樣，只是改了個順序

```cpp
struct ModelNodeInterface {
  virtual ~ModelNode() {}
  void addNode(ModelNode) = 0;
  void colorize() = 0;
}

struct ModelNode : ModelNodeInterface {
  void addNode(ModelNode) { ... }
  void colorize() { ... }
  ...
}
```

這個修改方向就無需特別修改，邊就能通過，因為 ModelNode 本身並沒有改變名稱

書中的說法是，如果有個好名字可以讓界面用，那就採取這個方案。如果想不到好名字，就採取上一個小節的方案

---

## 書中也有一些額外的細節跟討論

比較瑣碎，這邊就不記錄了

---

<!--
header: 25.11 引入實例委託
-->

# 25.11 引入實例委託

---

## 如果你這樣用

```cpp
public void someMethod() {
  BankingServices.updateAccountBalance(this->id, this->sum);
}
```

目標：讓外部決定該如何抽換 BankingServices.updateAccountBalance

---

## 程式設計師的摯友

`簡單函數抽取`！想要替換什麼，就把他丟到參數裡！

```cpp
public void someMethod(Fn<(int, Money) -> void> updateAccountBalance) {
  updateAccountBalance(this->id, this->sum);
}
```

外部就可以自由決定要替換為什麼

---

## 書裡面的解法

多長一個方法，然後再把類別丟到 someMethod 裡？

```cpp
struct BankingServices {
    static void updateAccountBalance(int, Money);
    
    void updateBalance(int id, Money money) {
        return BankingServices.updateAccountBalance(id, money);
    }
}

public void someMethod(BankingServices service) {
  service.updateBalance(this->id, this->sum);
}
```

外部的使用者可以去繼承 BankingServices 來複寫 updateBalance。以 2020 的眼光來看，似乎有些多此一舉

---

## 延伸閱讀

- [Objects are poor man's closures. Closures are poor man's objects.](https://stackoverflow.com/questions/2497801/closures-are-poor-mans-objects-and-vice-versa-what-does-this-mean)
  - 還記得現在很多語言都可以寫 lambda？連 Java 都可以
  
---

<!--
header: 25.12 引入靜態配置方法
-->

# 25.12 引入靜態配置方法

---

## Singleton

```cpp
struct S {
  static S& get() {
    if(!S::_instance) {
      S::_Instance.reset(new S{});
    }
    
    return *S::_instance;
  }
  
  // Other methods

private:
  static unique_ptr<S> _instance;
}
```

目標：為了測試那些用到 S 的程式碼，需要有個手段去控制 S::get 回來的東西

---

## 幫 singleton 增加 setter？

```cpp
struct S {
  ...
  
  static void set(unique_ptr<S> s) {
    s::_instance = std::move(s);
  }
  
  // Other methods

private:
  static unique_ptr<S> _instance;
}
```

繼承 S 後，透過 set 傳入 _instance 中，這樣就可以在需要的時候，抽換掉 S 的行為了！

---

## 用起來像是這樣

```cpp
void f() {
  S::get().xxx();
  S::get().ooo();
}

void test_case_for_f() {
    S::set(make_unique<FakeS>());
    f();
    S::set(nullptr);
}
```

---

## 他其實就是...

```cpp
S global_s;

void f() {
  global_s.xxx();
  global_s.ooo();
}

void test_case_for_f() {
    auto prev_s = std::move(global_s);
    global_s = FakeS{};
    f();
    global_s = prev_s;
}
```

偽裝地不錯的全域變數

---

## 勸說時間

- 如果可以，真的不要寫 singleton
- static 變數也請妥善使用，他只是範圍小一點的全域變數
- 你可以幫 S 做一個名字嚇人的靜態建構式，像是
  - `你真的不該怎麽做不論你有多好的理由這個物件只能被建立一次後果要自行負責`
  - 這樣，大家會在看完名字後感到緊張，知道不該隨意呼叫，就可以達到只有一個實體的目標了 （作者當然要寫一次建構子）
  - 然後使用函數參數的形式傳遞

---

## 略過書中的另一個例子

本小節的技巧基本上就是讓 singleton 變成完全的全域變數（可讀可寫），這邊就不多加贅述了

---

<!--
header: 25.13 連接替換
-->

# 25.13 連接替換

---

## 連接時期的功能抽換

書上是說，主要用在 C 上面

1. 寫一份你想要替換的程式碼實作
2. 修改你的建制系統，想辦法把你剛剛寫的東西塞進測試當中

---

<!--
header: 收尾
-->

# 收尾

---

## 為什麼硬要拆解那些物件導向的專屬修改方式？

因為我們的身邊開始出現 **不物件導向** 的程式語言了

`Go`，沒辦法繼承行為然後複寫
`Rust`，沒辦法繼承行為然後複寫
但是他們都還是可以寫測試，也沒有特別麻煩

或許我們該把重點從 **物件導向可以這樣做** 慢慢移動到 **我們想要達成的目標其實是XXX**，以這個觀點來閱讀這本書，相信可以獲得另一種樂趣

---

## 鮮少提及的世界：效果系統（effect system）

無論哪種通用程式語言，他們都要面對測試問題，面對抽換相依的問題

即使用上了依賴注入（DI），程式的撰寫還是沒有脫離 `參數傳遞` 這件事情，這裡來看看一些神奇的未來魔法長什麼樣子

- [wikipedia](https://en.wikipedia.org/wiki/Effect_system)
- [js-proposal-algebraic-effects](https://github.com/macabeus/js-proposal-algebraic-effects)
- [polysemy](https://github.com/polysemy-research/polysemy)
- [Algebraic Effects for the Rest of Us](https://overreacted.io/algebraic-effects-for-the-rest-of-us/) 

---

## 許願

希望可以有 **鑑賞** 性質的讀書會。把好的東西抄回來也不錯

---

<!--
header: ''
-->

# Q&A

---

# 感謝
