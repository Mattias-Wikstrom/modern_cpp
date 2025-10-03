#include <iostream>
#include <vector>
#include <string>
#include <numeric>
#include <algorithm>

namespace school {

// Forward declaration for mutual dependency
class MyStudent;

// ------------------ Student CRTP ------------------
template <typename DerivedStudent, typename CourseType>
class StudentCRTP {
public:
    explicit StudentCRTP(std::string name = "") : sName(std::move(name)) {}

    [[nodiscard]] const std::string& name() const noexcept { return sName; }

    void add(CourseType& c) {
        if (courses.size() < maxCourses && c.add(static_cast<DerivedStudent&>(*this)))
            courses.push_back(&c);
    }

    float grade(const CourseType& c) const {
        return c.grade(static_cast<const DerivedStudent&>(*this));
    }

    float grade() const {
        float total = 0.0f;
        int totalHours = 0;
        for (const auto* c : courses) {
            int h = c->hours();
            total += c->grade(static_cast<const DerivedStudent&>(*this)) * h;
            totalHours += h;
        }
        return (totalHours) ? (total / totalHours) : 0.0f;
    }

    float curvedGrade(const CourseType& c) const {
        float g = 3.0f * grade(c) / c.grade();
        return (g > 4.0f) ? 4.0f : g;
    }

private:
    static constexpr size_t maxCourses = 6;
    std::string sName;
    std::vector<CourseType*> courses;
};

// ------------------ Course CRTP ------------------
template <typename DerivedCourse, typename StudentType>
class CourseCRTP {
public:
    explicit CourseCRTP(int hours = 3) : semesterHours(hours) {}

    int hours() const noexcept { return semesterHours; }

    bool add(StudentType& s) {
        records.push_back({&s, 0.0f});
        return true;
    }

    void grade(StudentType& s, float g) {
        auto it = std::find_if(records.begin(), records.end(),
            [&s](const StudentRecord& r){ return r.student == &s; });
        if (it != records.end())
            it->grade = g;
    }

    float grade(const StudentType& s) const {
        auto it = std::find_if(records.begin(), records.end(),
            [&s](const StudentRecord& r){ return r.student == &s; });
        return (it != records.end()) ? it->grade : 0.0f;
    }

    float grade() const {
        if (records.empty()) return 0.0f;
        float sum = std::accumulate(records.begin(), records.end(), 0.0f,
            [](float acc, const StudentRecord& r){ return acc + r.grade; });
        return sum / static_cast<float>(records.size());
    }

private:
    int semesterHours{};

    struct StudentRecord {
        StudentType* student;
        float grade;
    };

    std::vector<StudentRecord> records;
};

// ------------------ Concrete types ------------------
class MyCourse : public CourseCRTP<MyCourse, MyStudent> {
public:
    using CourseCRTP::CourseCRTP; // inherit constructors
};

class MyStudent : public StudentCRTP<MyStudent, MyCourse> {
public:
    using StudentCRTP::StudentCRTP; // inherit constructors
};

} // namespace school

// ---------------------- main -------------------------
int main() {
    using namespace school;

    MyStudent harry("Harry");
    MyStudent anne("Anne");

    MyCourse geo101;
    MyCourse engl201(4);

    harry.add(geo101);
    anne.add(geo101);
    harry.add(engl201);

    geo101.grade(harry, 3.0f);
    geo101.grade(anne, 2.5f);
    engl201.grade(harry, 2.0f);

    std::cout << "Average geo101 grade   = " << geo101.grade() << "\n";
    std::cout << "Anne's grade           = " << anne.grade(geo101) << "\n";
    std::cout << "Anne's curved grade    = " << anne.curvedGrade(geo101) << "\n";
    std::cout << "Harry's average grade  = " << harry.grade() << "\n";

    return 0;
}
