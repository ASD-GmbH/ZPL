using FluentAssertions;
using NUnit.Framework;
using JanderIT;

namespace JanderIT.ZPL_Specs
{
    [TestFixture]
    public sealed class ZPL_specs
    {
        [Test]
        public void render_a_valid_ZPL_text_from_a_zpl_structure()
        {
            var demo = ZPL.render(ZPL.create(ZPL.top_section("hello", ZPL.kv("who", "world"), ZPL.section("welcome", ZPL.kv("who", "earth"))), ZPL.top_section("test", ZPL.kv("what", "data"))));
            const string expected = "hello\r\n    who=world\r\n    welcome\r\n        who=earth\r\ntest\r\n    what=data\r\n";

            demo.Should().Be(expected);
        }

        [Test]
        public void read_a_section_with_a_single_key_value_pair()
        {
            var demo = ZPL.render(ZPL.create(ZPL.top_section("hello", ZPL.kv("who", "world"))));
            ZPL.render(ZPL.parse(demo)).Should().Be(demo);
            ZPL.render(ZPL.parse_relaxed(demo)).Should().Be(demo);
        }

        [Test]
        public void read_a_complex_section()
        {
            var demo = ZPL.render(ZPL.create(ZPL.top_section("hello", ZPL.kv("who", "world"), ZPL.section("welcome", ZPL.kv("who", "earth"))), ZPL.top_section("test", ZPL.kv("what", "data"))));
            ZPL.render(ZPL.parse(demo)).Should().Be(demo);
            ZPL.render(ZPL.parse_relaxed(demo)).Should().Be(demo);
        }


        [Test]
        public void ignore_empty_lines()
        {
            const string demo = "hello\r\n    who=world\r\n    welcome\r\n        who=earth\r\ntest\r\n    what=data\r\n";
            const string demo1 = "hello\r\n\r\n    who=world\r\n    welcome\r\n\r\n        who=earth\r\n\r\ntest\r\n    what=data\r\n\r\n";
            ZPL.render(ZPL.parse(demo1)).Should().Be(demo);
            ZPL.render(ZPL.parse_relaxed(demo1)).Should().Be(demo);
        }

        [Test]
        public void ignore_hash_commented_lines()
        {
            const string demo = "hello\r\n    who=world\r\n    welcome\r\n        who=earth\r\ntest\r\n    what=data\r\n";
            const string demo1 = "#COMMENT\r\nhello\r\n    #COMMENT\r\n    who=world\r\n    welcome\r\n        who=earth\r\n  #COMMENT\r\ntest\r\n    what=data\r\n";
            ZPL.render(ZPL.parse(demo1)).Should().Be(demo);
            ZPL.render(ZPL.parse_relaxed(demo1)).Should().Be(demo);
        }

        [Test]
        public void ignore_doubleslash_commented_lines_in_relaxed_mode()
        {
            const string demo = "hello\r\n    who=world\r\n    welcome\r\n        who=earth\r\ntest\r\n    what=data\r\n";
            const string demo1 = "//COMMENT\r\nhello\r\n    //COMMENT\r\n    who=world\r\n    welcome\r\n        who=earth\r\n  //COMMENT\r\ntest\r\n    what=data\r\n";
            ZPL.render(ZPL.parse_relaxed(demo1)).Should().Be(demo);
        }

        [Test]
        public void ignore_cpp_style_block_comments_in_relaxed_mode()
        {
            const string demo = "hello\r\n    who=world\r\n    welcome\r\n        who=earth\r\ntest\r\n    what=data\r\n";
            const string demo1 = "hello\r\n    /*COMMENT\r\n MORE COMMENT*/\r\n    who=world\r\n    welcome\r\n        who=earth\r\n  test\r\n    what=data\r\n";
            ZPL.render(ZPL.parse_relaxed(demo1)).Should().Be(demo);
        }
    }
}
