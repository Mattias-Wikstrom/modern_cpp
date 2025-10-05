// Version of the students example suggested by Claude.ai; Uses dependency inversion

#include <iostream>
#include <vector>
#include <string>
#include <numeric>
#include <algorithm>
#include <optional>
#include <stdexcept>
#include <memory>

namespace school {

// ------------------ Abstract Interfaces ------------------
// These break the circular dependency

class IStudent {
public:
    virtual ~IStudent() = default;
    [[nodiscard]] virtual const std::string& name() const noexcept = 0;
};

class ICourse {
public:
    virtual ~ICourse() = default;
    [[nodiscard]] virtual int hours() const noexcept = 0;
};

// ------------------ Grade Record ------------------
struct GradeRecord {
    IStudent* student;
    float grade;
    
    GradeRecord(IStudent* s, float g = 0.0f) : student(s), grade(g) {}
};

// ------------------ Course (depends only on IStudent) ------------------
class Course : public ICourse {
public:
    explicit Course(int hours = 3) : semesterHours(hours) {
        if (hours <= 0) throw std::invalid_argument("Hours must be positive");
    }

    [[nodiscard]] int hours() const noexcept override { return semesterHours; }

    bool addStudent(IStudent& s) {
        // Check if student already enrolled
        auto it = std::find_if(records.begin(), records.end(),
            [&s](const GradeRecord& r){ return r.student == &s; });
        
        if (it != records.end()) return false;
        
        records.emplace_back(&s, 0.0f);
        return true;
    }
    
    void setGrade(const IStudent& s, float grade) {
        if (grade < 0.0f || grade > 4.0f) {
            throw std::invalid_argument("Grade must be between 0.0 and 4.0");
        }
        
        auto it = std::find_if(records.begin(), records.end(),
            [&s](const GradeRecord& r){ return r.student == &s; });
        
        if (it != records.end()) {
            it->grade = grade;
        }
    }
    
    [[nodiscard]] std::optional<float> getGrade(const IStudent& s) const {
        auto it = std::find_if(records.begin(), records.end(),
            [&s](const GradeRecord& r){ return r.student == &s; });
        
        return (it != records.end()) ? std::optional{it->grade} : std::nullopt;
    }
    
    [[nodiscard]] float averageGrade() const {
        if (records.empty()) return 0.0f;
        
        float sum = std::accumulate(records.begin(), records.end(), 0.0f,
            [](float acc, const GradeRecord& r){ return acc + r.grade; });
        
        return sum / static_cast<float>(records.size());
    }

private:
    int semesterHours;
    std::vector<GradeRecord> records;
};

// ------------------ Enrollment Record ------------------
struct Enrollment {
    ICourse* course;
    
    explicit Enrollment(ICourse* c) : course(c) {}
};

// ------------------ Student (depends only on ICourse) ------------------
class Student : public IStudent {
public:
    explicit Student(std::string name = "") : sName(std::move(name)) {}

    [[nodiscard]] const std::string& name() const noexcept override { return sName; }

    bool addCourse(Course& c) {
        if (enrollments.size() >= MAX_COURSES) return false;
        
        // Check if already enrolled
        if (std::find_if(enrollments.begin(), enrollments.end(),
            [&c](const Enrollment& e){ return e.course == &c; }) != enrollments.end()) {
            return false;
        }
        
        if (c.addStudent(*this)) {
            enrollments.emplace_back(&c);
            return true;
        }
        return false;
    }
    
    [[nodiscard]] std::optional<float> getGrade(const Course& c) const {
        return c.getGrade(*this);
    }
    
    [[nodiscard]] float gpa() const {
        float totalPoints = 0.0f;
        int totalHours = 0;
        
        for (const auto& enrollment : enrollments) {
            auto* course = dynamic_cast<Course*>(enrollment.course);
            if (course) {
                auto grade = course->getGrade(*this);
                if (grade) {
                    int hours = course->hours();
                    totalPoints += *grade * hours;
                    totalHours += hours;
                }
            }
        }
        
        return (totalHours > 0) ? (totalPoints / totalHours) : 0.0f;
    }
    
    [[nodiscard]] float curvedGrade(const Course& c) const {
        auto myGrade = c.getGrade(*this);
        if (!myGrade) return 0.0f;
        
        float courseAvg = c.averageGrade();
        if (courseAvg < 0.01f) return 0.0f;
        
        float curved = 3.0f * (*myGrade) / courseAvg;
        return std::min(curved, 4.0f);
    }

private:
    static constexpr size_t MAX_COURSES = 6;
    std::string sName;
    std::vector<Enrollment> enrollments;
};

} // namespace school

// ---------------------- main -------------------------
int main() {
    using namespace school;

    Student harry("Harry");
    Student anne("Anne");

    Course geo101;
    Course engl201(4);

    harry.addCourse(geo101);
    anne.addCourse(geo101);
    harry.addCourse(engl201);

    geo101.setGrade(harry, 3.0f);
    geo101.setGrade(anne, 2.5f);
    engl201.setGrade(harry, 2.0f);

    std::cout << "Average geo101 grade   = " << geo101.averageGrade() << "\n";
    std::cout << "Anne's grade           = " << anne.getGrade(geo101).value_or(0.0f) << "\n";
    std::cout << "Anne's curved grade    = " << anne.curvedGrade(geo101) << "\n";
    std::cout << "Harry's GPA            = " << harry.gpa() << "\n";

    return 0;
}
